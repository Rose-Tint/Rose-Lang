module Typing.Checker where

import Control.Monad ((<$!>), foldM, unless, when, forM_)

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
    infer (Array _ [] p) = updatePos p >>
        throw (OtherError "empty array")
    infer (Array _ (x:xs) p) = do
        updatePos p
        xT <- infer x
        pushExpType xT
        t <- foldM
            (\b a -> do
                aT <- infer $! a
                chk <- check aT xT
                if chk then
                    return $! b
                else
                    throw $! TypeMismatch aT xT
            ) xT xs
        popExpType
        return t


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
        _ <- case mDta of
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
        -- foldM (\_ a -> pushType
        --     (undef a) { sdType = Param a [] [] })
        --     () tps
        return NoType
    -- infer (IfElse cls tb fb) = do
    -- infer (Pattern val cs) = do
    -- infer (Loop init cond iter bdy) = do
    -- infer (TraitDecl vis cons name tv fns) = do
    --     return NoType
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
infer_ = optional . infer


checkAll :: (Checker a) => [a] -> Analyzer Bool
checkAll [] = return True
checkAll (x:xs) = do
    xT <- infer x
    foldM (\b a -> if not b then return b else do
        check a xT) True xs


expect :: (Checker a) => a -> Type -> Analyzer ()
{-# INLINABLE expect #-}
expect a t = withExpType t $! do
    typ <- infer a
    chk <- check typ t
    unless chk $! throw $
        TypeMismatch typ t
