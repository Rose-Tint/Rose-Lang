module Typing.Checker where

import Control.Monad (
    (<$!>),
    foldM,
    unless, when,
    forM_
    )
import Data.Array ((!), bounds)

import Analyzer.Analyzer
import Analyzer.Error
import Analyzer.Prims
import Analyzer.SymbolTable
import Parser.Data hiding (Type)
import SymbolTable
import Typing.Types


default (Int, Double)



class Checker a where
    infer :: a -> Analyzer Type
    check :: a -> Type -> Analyzer Bool
    check a expected = do
        typ <- infer a
        return $! (typ == expected)



instance Checker Type where
    infer t@(Delayed _) = do
        typ <- peekExpType
        return $! t <~> typ
    infer t = return t
    check t = return . (t ==)


instance Checker Value where
    infer (IntLit _ p) = updatePos p >>
        return intLitType
    infer (FltLit _ p) = updatePos p >>
        return fltLitType
    infer (ChrLit _ p) = updatePos p >>
        return chrLitType
    infer (StrLit _ p) = updatePos p >>
        return strLitType
    infer (FuncCall name args) = do
        updatePos $! varPos name
        dta <- searchScopeds name
        withExpType (sdType dta) $!
            apply (sdType dta) args
    infer (CtorVal name args) = do
        updatePos $! varPos name
        dta <- searchGlobals name
        withExpType (sdType dta) $!
            apply (sdType dta) args
    infer (ExprVal expr) = infer expr
    infer (Array arr p) = do
        updatePos p
        if snd (bounds arr) <= (0 :: Int) then
            throw $ OtherError "Empty array literal"
        else do
            typ <- infer (arr ! (0 :: Int))
            forM_ arr $ \t -> expect t typ
            return typ


instance Checker Expr where
    infer (ValueE val) = infer val
    infer (ModImport vis var) = do
        addImport vis var
        return NoType
    infer (FuncTypeDecl pur vis name cons typs) = do
        -- if this function-type-decl already exists, then
        -- check that they are the same (allow dupe-decls
        -- as long as they are the same)
        -- else, create a new global
        updatePos $! varPos name
        enterDefinition name
        mDta <- findGlobal name
        let typ' = addCons cons $! fromPDTypes typs
        case mDta of
            Nothing -> pushGlobal name $! mkSymbolData
                name typ' (Just vis) (Just pur)
            Just dta -> expect typ' (sdType dta)
        exitDefinition
        return NoType
    infer (FuncDef name pars _) = do
        updatePos $! varPos name
        enterDefinition name
        mDta <- findGlobal name
        case mDta of
            Nothing -> throwUndefined name
            Just dta -> do
                typ <- withExpType (sdType dta) $!
                    pushParams (sdType dta) pars
                return typ
        exitDefinition
        return NoType
    -- infer (DataDef vis name tps ctrs) = do
    infer (DataDef vis name tps ctrs) = do
        updatePos $! varPos name
        enterDefinition name
        let dta = mkSymbolData name
                (Type name (fmap
                    (\tp -> Param tp [] []) tps) [])
                (Just vis) (Just Pure)
        pushType name dta
        forM_ ctrs $ \(DataCtor vis' name' ts) -> do
            let typ = Applied
                    ((fromPDType <$!> ts) ++ [sdType dta]) []
            pushGlobal name' $! mkSymbolData
                name' typ (Just vis') (Just Pure)
        exitDefinition
        return NoType
    -- infer (IfElse cls tb fb) = do
    -- infer (Pattern val cs) = do
    -- infer (Loop init cond iter bdy) = do
    infer (TraitDecl vis _cons name _tv fns) = do
        updatePos $! varPos name
        enterDefinition name
        mDta <- findTrait name
        case mDta of
            Nothing -> pushTrait name $! mkSymbolData
                name NoType (Just vis) Nothing
            (Just dta) ->
                let nm = varName name
                    orig = maybe (Prim nm) (Var nm) (sdPos dta)
                in throw $! Redefinition name orig
        forM_ fns $! \fn -> case fn of
            FuncTypeDecl _ _ _ _ _ -> infer fn
            _ -> throw $! OtherError
                "non-function-type-declaration \
                \as trait-method declaration"
        exitDefinition
        return NoType
    -- infer (TraitImpl name cons typ defs) = do
    --     return NoType
    infer (NewVar mut typ var val) = do
        updatePos $! varPos var
        let typ' = fromPDType typ
        pushExpType typ'
        valT <- infer val
        chk <- check valT typ'
        popExpType
        if chk then do
            pushScoped var $! mkSymbolData
                var typ' (Just Intern) (Just mut)
            return NoType
        else
            throw $ TypeMismatch valT typ'
    infer (Reassign var val) = do
        var' <- findScoped var
        case var' of
            Nothing -> throwUndefined var
            Just dta -> do
                let varT = sdType dta
                pushExpType varT
                valT <- infer val
                chk <- check varT valT
                popExpType
                if chk then return NoType else throw $
                    TypeMismatch varT valT
    infer (Return val) = infer val
    infer _ = return NoType
    -- infer _ = throw $ OtherError
    --     "`Checker Expr` not fully implemented"


{-
ModImport -- WIP
FuncTypeDecl -- WIP
FuncDef -- WIP
DataDef -- WIP
IfElse -- WIP
Pattern -- WIP
Loop -- WIP
TraitDecl -- WIP
TraitImpl -- WIP
NewVar Mutability Type Variable Value
Reassign Variable Value
Return Value
-}

instance Checker SymbolData where
    infer = return . sdType


-- | the @typ@ in @`pushParams` typ ps@ represents the current
-- 'working' type (the type left). Remember to push the overall
-- function type using @pushExpType@
pushParams :: Type -> [Value] -> Analyzer Type
pushParams typ [] = return typ
pushParams typ@(Applied tps cs) (param:params) = do
    updatePos $! valPos param
    when (null tps) $! do
        eT <- peekExpType
        throw $ TypeMismatch typ eT
    let (eT:tps') = tps
    _ <- case param of
        FuncCall var [] -> do
            updatePos $! varPos var
            let dta = mkSymbolData var eT Nothing Nothing
            pushScoped var dta
            return ()
        FuncCall _ _ -> fail
            "pattern-match variable case cannot have arguments"
        CtorVal name _ -> do
            modifyGlobal name $! \dta ->
                case sdType dta of
                    NoType -> dta { sdType = eT }
                    Delayed _ -> dta { sdType = eT }
                    _ -> dta
            -- dTyp <- sdType <$!> searchGlobals name
            -- let dTyp' = dTyp <~> eT
            -- withExpType dTyp' $! pushParams dTyp' vals
            return ()
        -- TODO: Literals
        _ -> return ()
    pushParams (Applied tps' cs) params
pushParams (Type _ _ _) _ = fail "`Type` in pushParams"
pushParams (Param _ _ _) _ = fail "`Param` in pushParams"
pushParams typ@(Delayed _) ps = do
    typ' <- infer typ
    case typ' of
        Delayed _ -> fail "unavoidable `Delayed` in pushParams"
        _ -> pushParams typ' ps
pushParams NoType _ = fail "`NoType` in pushParams"


apply :: Type -> [Value] -> Analyzer Type
apply ft [] = return ft
apply ft (val:vals) = do
    updatePos $! valPos val
    valT <- infer val
    expT <- peekExpType
    areSame <- check valT expT
    if areSame then
        apply ft vals
    else case val of
        -- areSame will be True if expT is Nothing,
        -- so fromJust is safe here
        FuncCall _ _ -> throw $
            TypeMismatch valT expT
        _ -> throw $
            TypeMismatch valT expT


infer_ :: (Checker a) => a -> Analyzer ()
{-# INLINE infer_ #-}
infer_ = optional . infer


checkAll :: (Checker a) => [a] -> Analyzer Bool
{-# INLINE checkAll #-}
checkAll [] = return True
checkAll (x:xs) = do
    xT <- infer x
    foldM (\b a -> if not b then return b else do
        check a xT) True xs


expect :: (Checker a) => a -> Type -> Analyzer ()
{-# INLINE expect #-}
expect a t = withExpType t $! do
    typ <- infer a
    chk <- check typ t
    unless chk $! throw $
        TypeMismatch typ t
