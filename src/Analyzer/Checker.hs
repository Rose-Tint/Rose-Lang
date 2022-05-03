module Analyzer.Checker (
    Checker(..),
    infer_,
) where

import Prelude hiding (fail)

import Control.Monad (foldM)
import Data.Array (elems)

import Analyzer.Analyzer
import Analyzer.Error
import Analyzer.SymbolTable
import Common.Typing
import Parser.Data
import SymbolTable


default (Int, Double)


class Checker a where
    infer :: a -> Analyzer Type
    -- |Checks that something is infered to be
    -- compatable with the given type
    isOfType :: a -> Type -> Analyzer Bool
    isOfType a expected = do
        typ <- infer a
        return (typ == expected)


instance Checker Type where
    infer Delayed = peekExpType
    infer t = return t
    isOfType t = return . (t ==)

instance Checker Value where
    infer (IntLit _ p) = updatePos p >>
        return intType
    infer (FloatLit _ p) = updatePos p >>
        return floatType
    infer (DoubleLit _ p) = updatePos p >>
        return doubleType
    infer (CharLit _ p) = updatePos p >>
        return charType
    infer (StringLit _ p) = updatePos p >>
        return stringType
    infer (VarVal var) = do
        updatePosVar var
        sdType <$> searchScopeds var
    infer (Application val vals) =
        Applied <$> mapM infer (val:vals)
    infer (CtorCall name args) = do
        updatePosVar name
        typ <- sdType <$> searchGlobals name
        expect typ (apply typ args)
    infer (Tuple arr) = do
        typs <- elems <$> mapM infer arr
        return $! tupleOf typs
    infer (Array arr) = do
        eT <- peekExpType
        typ <- foldM (\bT val -> do
            vT <- infer val
            case bT <:> vT of
                NoType -> throw $ TypeMismatch bT vT
                typ -> return typ
            ) eT arr
        return $! arrayOf typ
    infer (Lambda params bdy) = do
        let pTs = (Delayed:(Delayed <$ params))
            apT = Applied pTs
        bT <- expect apT (inferBody bdy)
        return (Applied (tail pTs ++ [bT]))
    infer (StmtVal stmt) = infer stmt
    infer (Hole p) = updatePos p >> peekExpType

instance Checker Stmt where
    infer (IfElse cls trueBody falseBody) = do
        expectCheck_ boolType cls
        tT <- inferBody trueBody
        fT <- inferBody falseBody
        return (tT <::> fT)
    infer _ = return NoType
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
--             chk <- isOfType valT typ'
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

instance Checker Expr where
    infer _ = return NoType

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
--             -- let dTyp' = dTyp <:> eT
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

apply :: Type -> [Value] -> Analyzer Type
apply ft [] = return ft
apply ft (val:vals) = do
    updatePos (valPos val)
    vT <- infer val
    eT <- peekExpType
    areSame <- isOfType vT eT
    if areSame then
        apply ft vals
    else 
        throw $ TypeMismatch vT eT

infer_ :: Checker a => a -> Analyzer ()
infer_ = optional . infer

inferBody :: Body -> Analyzer Type
inferBody body = do
    eT <- peekExpType
    foldM expectCheck eT body

expectCheck_ :: Checker a => Type -> a -> Analyzer ()
expectCheck_ typ a = do
    aT <- expect typ (infer a)
    isOfType typ aT
    return ()

expectCheck :: Checker a => Type -> a -> Analyzer Type
expectCheck typ a = do
    aT <- expect typ (infer a)
    if typ == NoType then
        return aT
    else do
        same <- isOfType aT typ
        if same then
            return (typ <:> aT)
        else
            throw $ TypeMismatch typ aT
