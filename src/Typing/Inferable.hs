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
import Typing.Infer
import Typing.Primitives
import Typing.Scheme
import Typing.Solver
import Typing.Type
import Typing.TypeEnv
import Typing.TypeDecl


makeInference :: Inferable a =>
    TypeEnv -> a -> Either Error Scheme
makeInference env a = case runInfer env (infer a) of
    Left err -> Left err
    Right (cons, typ) -> runSolver typ cons


class Inferable a where
    infer :: a -> Infer Type


instance Inferable Literal where
    infer IntLit{} = return intType
    infer FloatLit{} = return floatType
    infer DoubleLit{} = return doubleType
    infer CharLit{} = return charType
    infer StringLit{} = return stringType

instance Inferable Value where
    infer (Literal lit) = infer lit
    infer (VarVal name) = searchEnv name
    infer (Application v1 v2) = do
        t1 <- infer v1
        t2 <- infer v2
        tv <- fresh
        constrain t1 (t2 :-> tv)
        return tv
    infer (CtorCall name) = searchEnv name
    infer (Lambda ps body) = do
        env <- ask
        tv <- fresh
        (psT, env') <- foldM (\(psT, env1) p -> do
            tv' <- fresh
            let scheme = Forall [] tv'
                env2 = extend p scheme env1
            return ((tv :-> psT), env2)
            ) (tv, env) ps
        bT <- local (const env') (infer body)
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
    infer (Param name) = searchEnv name
    infer Hole{} = fresh
    infer (CtorPtrn name []) = searchEnv name
    infer (CtorPtrn name (arg:args)) = do
        t1 <- searchEnv name
        t2 <- applyPtrns arg args
        constrain t2 t1
        return (t2 :-> t1)
    infer (TuplePtrn args) =
        TupleType <$> mapM infer args
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
    etb <- inferStmt trueBody
    efb <- inferStmt falseBody
    mergeStmtInfs etb efb
inferStmt (Loop init' cond iter body) = do
    inferStmt init'
    case cond of
            ValStmt val -> do
                cT <- infer val
                constrain cT boolType
            _ -> error "invalid condition expression"
    inferStmt iter
    inferStmt body
    return Nothing
inferStmt (Match val cases) = do
    vT <- infer val
    inferCases vT cases
inferStmt (NewVar _mut _name (TypeDecl _ typ) val) = do
    vT <- infer val
    constrain typ vT
    return Nothing
inferStmt (Reassignment name val) = do
    vT <- infer val
    nT <- searchEnv name
    constrain vT nT
    return Nothing
inferStmt (Return val) = Just <$> infer val
inferStmt (ValStmt val) = do
    infer val
    return Nothing
inferStmt (Compound body) = do
    foldM (\b stmt -> do
        i <- inferStmt stmt
        mergeStmtInfs b i
        ) Nothing body
inferStmt _ = return Nothing

inferCases :: Type -> [MatchCase] -> Infer (Maybe Type)
inferCases _ [] = return Nothing
inferCases vT cases = foldM (\prev (Case ptrn body) -> do
    ptT <- infer ptrn
    constrain ptT vT
    mbT <- inferStmt body
    mergeStmtInfs prev mbT
    ) Nothing cases

-- if both bodies guarantee a return, then this can
-- as well. otherwise it cannot be guaranteed.
mergeStmtInfs :: Maybe Type -> Maybe Type -> Infer (Maybe Type)
mergeStmtInfs Nothing Nothing = return Nothing
mergeStmtInfs (Just typ) Nothing = return (Just typ)
mergeStmtInfs Nothing (Just typ) = return (Just typ)
mergeStmtInfs (Just t1) (Just t2) = do
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
