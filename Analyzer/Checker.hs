module Analyzer.Checker (
    Checker(..),
    infer_,
) where

import Prelude hiding (fail)

import Analyzer.Analyzer
import Analyzer.Prims
import Parser.Data
import SymbolTable


default (Int, Double)


class Checker a where
    infer :: a -> Analyzer Type
    check :: a -> Type -> Analyzer Bool
    check a expected = do
        typ <- infer a
        return (typ == expected)


instance Checker Type where
    infer Delayed = peekExpType
    infer t = t
    check t = return . (t ==)

instance Checker Value where
    infer (IntLit _ p) = updatePos p >>
        return intLitType
    infer (FltLit _ p) = updatePos p >>
        return floatLitType
    infer (ChrLit _ p) = updatePos p >>
        return charLitType
    infer (StrLit _ p) = updatePos p >>
        return stringLitType
    infer (VarVal var) = do
        updatePosVar var
        sdType <$> searchScopeds var
    infer (Application val vals) =
        Applied <$> mapM infer (val:vals)
    infer (CtorCall name vals) = do
        updatePosVar name
        dta <- searchGlobals name
        expect typ (apply typ args)
    infer (Tuple arr) = do
        typs <- mapM infer arr
        return $! tupleOf typs
    infer (Array arr) = do
        eT <- peekExpType
        typ <- foldM (\bT val -> do
            vT <- infer val
            typ <- bT <~> vT
            case typ of
                NoType -> throw $
                    TypeMismatch bT vT
                _ -> return typ
            ) eT arr
        return $! arrayOf typ
    infer (Lambda params val) = do
        let pTs = Delayed <$ params
            apT = Applied (pTs:Delayed)
        vT <- expect apT (infer val))
        return (Applied (pTs:vT))
    infer (StmtVal stmt) = infer stmt
    infer (Hole p) = updatePos p >> peekExpType

instance Checker Stmt where
    infer _ = return NoType
    infer (IfElse cls trueBody falseBody) = do
        expectCheck_ boolType cls
        tT <- inferBody trueBody
        fT <- inferBody falseBody
        check tT fT
        return NoType
--     infer (Pattern val cases) = do
--         valT <- infer val
--         forM_ cases $ \(case', bdy) -> do
--             expect case' valT
--             mapM_ infer_ bdy
--         return NoType
--     infer (Loop init' cond iter bdy) = do
--         maybe (return ()) infer_ init'
--         expect cond boolType
--         maybe (return ()) infer_ iter
--         mapM_ infer bdy
--         return NoType
--     -- TraitDecl vis cons tv fns
--     infer (TraitDecl vis _ name _ fns) = define name $! do
--         mDta <- findTrait name
--         case mDta of
--             Nothing -> pushTrait name $ mkSymbolData
--                 name NoType (Just vis) Nothing
--             Just dta ->
--                 let nm = varName name
--                     orig = maybe (Prim nm) (Var nm)
--                         (sdPos dta)
--                 in throw $ Redefinition name orig
--         forM_ fns $ \fn -> case fn of
--             FuncTypeDecl _ _ _ _ _ -> infer fn
--             Pragma _ -> return NoType
--             _ -> fail
--                 "non-function-type-declaration \
--                 \as trait-method declaration"
--     -- TraitImpl name cons typ defs
--     infer (TraitImpl name _ _ defs) = define name $! do
--             mDta <- findTrait name
--             case mDta of
--                 Nothing -> throwUndefined name
--                 Just _ -> mapM_ infer_ defs
--     infer (NewVar mut typ var val) = do
--         updatePos $ varPos var
--         let typ' = fromPDType typ
--         (valT, chk) <- expectIn typ' $ do
--             valT <- infer val
--             chk <- check valT typ'
--             return (valT, chk)
--         if chk then do
--             pushScoped var $ mkSymbolData
--                 var typ' (Just Intern) (Just mut)
--             return NoType
--         else
--             throw $ TypeMismatch valT typ'
--     infer (Reassign var val) = do
--         var' <- findScoped var
--         case var' of
--             Nothing -> throwUndefined var
--             Just dta -> do
--                 expect val (sdType dta)
--                 return NoType
--     infer (Return val) = infer val

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


-- -- | the @typ@ in @`pushParams` typ ps@ represents the
-- -- current 'working' type (the type left). Remember to
-- -- push the overall function type using @`expectIn`@
-- pushParams :: Type -> [Value] -> Analyzer Type
-- pushParams typ [] = return typ
-- pushParams typ@(Applied tps ) (param:params) = do
--     updatePos $ valPos param
--     when (null tps) $ do
--         eT <- peekExpType
--         throw $ TypeMismatch typ eT
--     let (eT, tps') = ((head tps), (tail tps))
--     _ <- case param of
--         VarVal var -> do
--             updatePos $ varPos var
--             let dta = mkSymbolData var eT Nothing Nothing
--             pushScoped var dta
--             return ()
--         CtorCall name _ -> do
--             modifyGlobal name $ \dta ->
--                 case sdType dta of
--                     Delayed -> dta { sdType = eT }
--                     _ -> dta
--             -- dTyp <- sdType <$!> searchGlobals name
--             -- let dTyp' = dTyp <~> eT
--             -- expectIn dTyp' $! pushParams dTyp' vals
--             return ()
--         Hole pos -> do
--             updatePos pos
--             return ()
--         -- TODO: Literals
--         _ -> return ()
--     pushParams (Applied tps') params
-- pushParams (Type _ _) _ = fail "`Type` in pushParams"
-- pushParams NoType _ = fail "`NoType` in pushParams"
-- pushParams typ ps = do
--     typ' <- infer typ
--     case typ' of
--         Delayed -> fail
--             "unavoidable `Delayed` in pushParams"
--         _ -> expectIn typ' $ pushParams typ' ps

-- apply :: Type -> [Value] -> Analyzer Type
-- apply ft [] = return ft
-- apply ft (val:vals) = do
--     updatePos $ valPos val
--     valT <- infer val
--     expT <- peekExpType
--     areSame <- check valT expT
--     if areSame then
--         apply ft vals
--     else 
--         throw $ TypeMismatch valT expT

infer_ :: Checker a => a -> Analyzer ()
{-# INLINE infer_ #-}
infer_ = optional . infer

inferBody :: Body -> Analyzer Type
inferBody body = do
    eT <- peekExpType
    foldM expectCheck eT body

expectCheck_ :: Checker a -> Type -> Analyzer a -> Analyzer ()
expectCheck_ typ an = do
    expect typ an
    check typ an

expectCheck :: Checker a -> Type -> Analyzer a -> Analyzer Type
expectCheck typ an = do
    aT <- expect typ an
    if typ == NoType then
        return aT
    else do
        chk <- check an typ
        if chk then
