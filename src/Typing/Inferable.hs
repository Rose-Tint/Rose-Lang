{-# LANGUAGE LambdaCase #-}

module Typing.Inferable (
    makeInference,
    infer,
    inferStmt,
) where

import Prelude hiding (lookup)

import Control.Monad (foldM)
import qualified Data.Array as A (elems)

import AST
import Analysis.Error
import Data.Table
import Typing.Infer
import Typing.Primitives
import Typing.Scheme
import Typing.Solver
import Typing.Type
import Typing.TypeDecl


makeInference :: Inferable a => Table -> a
    -> Either Error Scheme
makeInference env a = case runInfer env (infer a) of
    Left err -> Left err
    Right (typ, cons, _tbl) -> runSolver typ cons


class Inferable a where
    infer :: a -> Infer Type


instance Inferable Type where
    infer (Type name tps) =
        Type name <$> mapM infer tps
    infer TypeVar{} = fresh
    infer (t1 :-> t2) = do
        t1' <- infer t1
        t2' <- infer t2
        tv <- fresh
        constrain t1' (t2' :-> tv)
        return tv
    infer (TupleType types) =
        TupleType <$> mapM infer types
    infer (ArrayType typ) = ArrayType <$> infer typ

instance Inferable Literal where
    infer IntLit{} = return intType
    infer FloatLit{} = return floatType
    infer DoubleLit{} = return doubleType
    infer CharLit{} = return charType
    infer StringLit{} = return stringType

instance Inferable Value where
    infer (Literal lit) = infer lit
    infer (VarVal name) = searchScopeds name
    infer (Application v1 v2) = do
        t1 <- infer v1
        t2 <- infer v2
        tv <- fresh
        constrain t1 (t2 :-> tv)
        return tv
    infer (CtorCall name) = searchGlobals name
    infer (Lambda ps body) = do
        (psT, bT) <- inNewScope $ do
            tv <- fresh
            psT <- foldM (\psT p -> do
                tv' <- fresh
                pushParam p tv'
                -- TODO: should this be switched??
                constrain tv' psT
                return (tv' :-> psT)
                ) tv ps
            bT <- infer body
            return (psT, bT)
        constrain psT bT
        return (psT :-> bT)
    infer (Tuple arr) =
        tupleOf <$> mapM infer (A.elems arr)
    infer (Array arr) = do
        tv <- fresh
        arrayOf <$> foldM (\t1 val -> do
            t2 <- infer val
            constrain t1 t2
            return t2
            ) tv arr
    infer (IfElseVal cond tr fa) = do
        cT <- infer cond
        tT <- infer tr
        fT <- infer fa
        constrain cT boolType
        constrain tT fT
        return tT
    infer (MatchVal val cases) = do
        vT <- infer val
        tv <- fresh
        foldM (\prT (ptrn, body) -> do
            ptT <- infer ptrn
            bT <- infer body
            constrain ptT vT
            constrain bT prT
            return bT
            ) tv cases

instance Inferable Pattern where
    infer (Param name) = do
        tv <- fresh
        pushParam name tv
        return tv
    infer Hole{} = fresh
    infer (CtorPtrn name []) = searchGlobals name
    infer (CtorPtrn name (arg:args)) = do
        t1 <- searchGlobals name
        t2 <- applyPtrns arg args
        tv <- fresh
        constrain t1 (t2 :-> tv)
        return (t2 :-> t1)
    infer (TuplePtrn args) =
        tupleOf <$> mapM infer args
    infer (LitPtrn lit) = infer lit
    infer (OrPtrn p1 p2) = do
        t1 <- infer p1
        t2 <- infer p2
        constrain t2 t1
        return t2


-- | Returns `Just t` only if it can guarantee
-- that it will return (i.e. all cases guarantee a
-- return in a `Match`). If no such guarantee can be
-- made, it will return `Nothing`.
inferStmt :: Stmt -> Infer (Maybe Type)
inferStmt (IfElse cond trueBody falseBody) = do
    cT <- infer cond
    constrain cT boolType
    etb <- allowJumpsIn $ inferStmt trueBody
    efb <- allowJumpsIn $ inferStmt falseBody
    mergeStmts etb efb
inferStmt (Loop init' cond iter body) = do
    inferStmt init'
    case cond of
        ValStmt val -> do
            cT <- infer val
            constrain cT boolType
        _ -> throw $ OtherError
            "invalid condition expression"
    inferStmt iter
    allowJumpsIn $ inferStmt body
    return Nothing
inferStmt (Match val cases) = do
    vT <- infer val
    allowJumpsIn $ inferCases vT cases
inferStmt (NewVar mut name (TypeDecl _ typ) val) = do
    -- 'cleanses' the type
    typ' <- infer typ
    pushScoped mut name typ'
    vT <- infer val
    constrain vT typ'
    return Nothing
inferStmt (Reassignment name val) = do
    vT <- infer val
    nT <- findScoped name
    constrain vT nT
    return Nothing
inferStmt (Return val) = Just <$> infer val
inferStmt (ValStmt val) = do
    infer val
    return Nothing
inferStmt (Compound body) =
    foldM (\b stmt -> do
        i <- inferStmt stmt
        mergeStmts b i
        ) Nothing body
inferStmt Break = do
    legal <- gets jumpAllowed
    if not legal then
        throw $ OtherError "illegal `break`"
    else
        return Nothing
inferStmt Continue = do
    legal <- gets jumpAllowed
    if not legal then
        throw $ OtherError "illegal `continue`"
    else
        return Nothing
inferStmt NullStmt = return Nothing

inferCases :: Type -> [MatchCase] -> Infer (Maybe Type)
inferCases _ [] = return Nothing
inferCases vT cases =
    foldM (\prev (Case ptrn body) -> do
        ptT <- infer ptrn
        constrain ptT vT
        mbT <- inferStmt body
        mergeStmts prev mbT
    ) Nothing cases

-- if both bodies guarantee a return, then this can
-- as well. otherwise it cannot be guaranteed.
mergeStmts :: Maybe Type -> Maybe Type
    -> Infer (Maybe Type)
mergeStmts Nothing Nothing = return Nothing
mergeStmts (Just typ) Nothing = return (Just typ)
mergeStmts Nothing (Just typ) = return (Just typ)
mergeStmts (Just t1) (Just t2) = do
    constrain t2 t1
    return (Just t2)

applyPtrns :: Pattern -> [Pattern] -> Infer Type
applyPtrns val [] = infer val
applyPtrns v1 (v2:vs) = do
    t1 <- infer v1
    t2 <- applyPtrns v2 vs
    tv <- fresh
    let typ = t2 :-> tv
    constrain t1 typ
    return typ
