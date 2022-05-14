{-# LANGUAGE LambdaCase #-}

module Typing.Inferable (
    Inference(..),
    makeInference,
    -- infer,
    inferExpr,
) where

import Prelude hiding (lookup)

import Control.Monad (foldM)

import Common.Var
import AST
import Analysis.Error
import Typing.Infer
import Typing.Primitives
import Typing.Scheme
import Typing.Substitution
import Typing.Type
import Typing.TypeEnv
import Typing.TypeDecl
import Typing.Unification


data Inference = Inf [ErrInfo] TypeEnv


makeInference :: [Expr] -> Inference
makeInference exprs = Inf errs env
    where
        (_, errs, env) = runInfer inf
        inf = do
            mapM_ (\expr ->
                -- refresh >> inferExpr expr
                inferExpr expr
                ) exprs
            return emptyEnv
            -- foldM (\env1 ex -> do
            -- env2 <- inferExpr ex
            -- return (env1 `union` env2)
            -- ) emptyEnv exprs


class Inferable a where
    infer :: a -> Infer (Subst, Type)


instance Inferable Literal where
    infer IntLit{} = return (nullSubst, intType)
    infer FloatLit{} = return (nullSubst, floatType)
    infer DoubleLit{} = return (nullSubst, doubleType)
    infer CharLit{} = return (nullSubst, charType)
    infer StringLit{} = return (nullSubst, stringType)

instance Inferable Value where
    infer (Literal lit) = infer lit
    infer (VarVal name) = searchEnv name
    infer (Application v1 v2) = do
        tv <- fresh
        (s1, t1) <- infer v1
        applyEnv s1
        (s2, t2) <- infer v2
        s3 <- unify (apply s2 t1) (t2 :-> tv)
        return (s3 <|> s2 <|> s1, apply s3 tv)
    infer (CtorCall name) = searchEnv name
    infer (Lambda ps body) = do
        tv <- fresh
        pushParams ps
        (bS, bT) <- infer body
        return (bS, apply bS (tv :-> bT))
    infer (Tuple arr) = do
        (sub, types) <- foldM (\(s1, types) val -> do
            (s2, typ) <- infer val
            return (s2 <|> s1, (typ:types))
            ) (nullSubst, []) arr
        return (sub, apply sub (tupleOf (reverse types)))
    infer (Array arr) = do
        tv <- fresh
        (sub, typ) <- foldM (\(s1, t1) val -> do
            (s2, t2) <- infer val
            s3 <- unify t1 t2
            return (s3 <|> s2 <|> s1, t1)
            ) (nullSubst, tv) arr
        return (sub, apply sub (arrayOf typ))
    infer (IfElseVal cond tr fa) = do
        (cS, cT) <- infer cond
        (tS, tT) <- infer tr
        (fS, fT) <- infer fa
        cS' <- unify cT boolType
        bS' <- unify tT fT
        return (compose [bS',cS',fS,tS,cS], apply bS' tT)
    infer (MatchVal val cases) = do
        tv <- fresh
        (vS, vT) <- infer val
        foldM (\(prS, prT) (ptrn, body) -> do
            (ptS, ptT) <- infer ptrn
            (bS, bT) <- infer body
            vpS <- unify vT ptT
            pbS <- unify prT bT
            return (compose [pbS, vpS, bS, ptS, prS], bT)
            ) (vS, tv) cases

instance Inferable Pattern where
    infer (Param name) = searchEnv name
    infer Hole{} = do
        tv <- fresh
        return (nullSubst, tv)
    infer (CtorPtrn name []) = searchEnv name
    infer (CtorPtrn name (arg:args)) = do
        (s1, t1) <- searchEnv name
        (s2, t2) <- applyPtrns arg args
        s3 <- unify t1 t2
        return (s3 <|> s2 <|> s1, t2 :-> t1)
    infer (TuplePtrn args) = do
        (sub, types_) <- foldM (\(s1, types) arg -> do
            (s2, typ) <- infer arg
            return (s2 <|> s1, (typ:types))
            ) (nullSubst, []) args
        return (sub, TupleType (reverse types_))
    infer (LitPtrn lit) = infer lit
    infer (OrPtrn p1 p2) = do
        (s1, t1) <- infer p1
        (s2, t2) <- infer p2
        s3 <- unify t1 t2
        return (s3 <|> s2 <|> s1, apply s3 t1)


-- | Returns `Right (s, t)` only if it can guarantee
-- that it will return (i.e. all cases guarantee a
-- return in a `Match`). If no such guarantee can be
-- made, it will return `Left s`.
--
-- In short:
-- - `Left` -> return not guaranteed, but here are
--     the substitutions i found anyway
-- - `Right` -> return guaranteed! Here are the
--     substitutions and type being returned.
inferStmt :: Stmt -> Infer (Either Subst (Subst, Type))
inferStmt (IfElse cond trueBody falseBody) = do
    (_cS, cT) <- infer cond
    _cS' <- unify boolType cT
    etb <- inferStmt trueBody
    efb <- inferStmt falseBody
    mergeStmtInfs etb efb
inferStmt (Loop init' cond iter body) = do
    inS <- inferStmtSubst init'
    cS <- inferCond
    itS <- inferStmtSubst iter
    bS <- inferStmtSubst body
    let sub = compose [bS,itS,cS,inS]
    return (Left sub)
    where
        inferCond = case cond of
            ValStmt val -> do
                (cS, cT) <- infer val
                sub <- unify cT boolType
                return (sub <|> cS)
            _ -> do
                throw $ OtherError
                    "invalid condition expression"
                return nullSubst
inferStmt (Match val cases) = do
    vI <- infer val
    inferCases vI cases
inferStmt (NewVar _mut _name (TypeDecl _ typ) val) = do
    (vS, vT) <- infer val
    sub <- unify typ vT
    return (Right (sub <|> vS, apply sub typ))
inferStmt (Reassignment _ val) = do
    (s, _) <- infer val
    return (Left s)
inferStmt (Return val) = Right <$> infer val
inferStmt (ValStmt val) = do
    (s, _) <- infer val
    return (Left s)
inferStmt (Compound body) = do
    foldM (\b stmt -> do
        i <- inferStmt stmt
        mergeStmtInfs b i
        ) (Left nullSubst) body
inferStmt _ = return (Left nullSubst)

inferCases ::
    -- | The result of inferring from value being matched
    (Subst, Type)
    -> [MatchCase] -> Infer (Either Subst (Subst, Type))
inferCases (vS, _) [] = return (Left vS)
inferCases (vS, vT) cases = do
    tv <- fresh
    foldM (\prev (Case ptrn body) -> do
        (pS, pT) <- infer ptrn
        vpS <- unify vT pT
        case prev of
            Left s1 -> do
                s2 <- inferStmtSubst body
                return (Left (compose [vpS,s2,pS,s1]))
            Right (s1, typ) -> inferStmt body >>= \case
                Left bS -> return
                    (Left (compose [bS, vpS, pS, s1]))
                Right (bS, bT) -> do
                    s2 <- unify typ bT
                    let s3 = compose
                            [s2, bS, vpS, pS, s1]
                    return (Right (s3, bT))
        ) (Right (vS, tv)) cases

-- if both bodies guarantee a return, then this can
-- as well. otherwise it cannot be guaranteed.
mergeStmtInfs ::
    Either Subst (Subst, Type) ->
    Either Subst (Subst, Type) ->
    Infer (Either Subst (Subst, Type))
mergeStmtInfs (Left s1) (Left s2) = return
    (Left (s1 <|> s2))
mergeStmtInfs (Left s1) (Right (s2, t)) = return
    (Right (s1 <|> s2, t))
mergeStmtInfs (Right (s1, t)) (Left s2) = return
    (Right (s1 <|> s2, t))
mergeStmtInfs (Right (s1, t1)) (Right (s2, t2)) = do
    s3 <- unify t1 t2
    return (Right (s1 <|> s2 <|> s3, t1))

inferStmtSubst :: Stmt -> Infer Subst
inferStmtSubst stmt = do
    eith <- inferStmt stmt
    return (either id fst eith)

inferParams :: [Pattern] -> Infer (Subst, Type)
inferParams [] = do
    tv <- fresh
    return (nullSubst, tv)
inferParams (p:ps) = do
    tv <- fresh
    (s1, t1) <- infer p
    (s2, t2) <- local (apply s1) (inferParams ps)
    s3 <- unify (apply s2 t1) (t2 :-> tv)
    return (s3 <|> s2 <|> s1, apply s3 tv)

inferExpr :: Expr -> Infer ()
inferExpr (FuncDecl _ _ name typDcl) = do
    env <- getEnv
    let TypeDecl _cons typ = typDcl
        scheme = generalize env typ
    extendEnv name scheme
inferExpr (DataDef _ name pars _ctors) = do
    let tvs = TypeVar <$> pars
        scheme = Forall pars (Type name tvs)
    extendEnv name scheme
-- TODO:
inferExpr (TraitDecl _ _ctx _name _pars fns) = do
    -- let tvs = TypeVar <$> pars
        -- scheme = Forall tvs ...
    -- local (apply tvs) (...
    mapM_ inferExpr fns
-- TODO:
inferExpr (TraitImpl _ctx _name _types _fns) = do
    -- let Forall tvs typ = searchEnv name env
    -- foldM inferExpr env fns
    return ()
inferExpr (FuncDef name pars body) = do
    env <- getEnv
    (fnS, fnT) <- searchEnv name
    (psS, psT) <- inferParams pars
    fnS' <- unify (apply psS fnT) psT
    bodyInf <- local (apply fnS) (inferStmt body)
    case bodyInf of
        Right (bS, bT) -> do
            fbS <- unify bT fnT
            let sub = compose [fbS, bS, fnS, psS, fnS']
                typ = apply sub fnT
                scheme = generalize env typ
            extendEnv name scheme
        Left _ -> throw (MissingReturn name)
inferExpr (TypeAlias _ _name _typ) = do
    return ()

pushParams :: [Var] -> Infer ()
pushParams [] = return ()
pushParams (par:pars) = do
    tv <- fresh
    extendEnv par (Forall [] tv)
    pushParams pars

applyPtrns :: Pattern -> [Pattern] -> Infer (Subst, Type)
applyPtrns val [] = infer val
applyPtrns v1 (v2:vs) = do
    tv <- fresh
    (s1, t1) <- infer v1
    (s2, t2) <- local (apply s1) (applyPtrns v2 vs)
    s3 <- unify (apply s2 t1) (t2 :-> tv)
    return (s3 <|> s2 <|> s1, apply s3 tv)
