{-# LANGUAGE FlexibleInstances #-}

module Middle.Typing.Inferable (
    infer,
    fresh,
    lookupEnv,
    unify,
    bind,
    instantiate,
    generalize,
    newTypeEnv,
    inferNew,
) where

import Prelude hiding (lookup)

import Control.Monad
import qualified Data.Set as S

import Common.Typing
import Common.Var
import Front.Parser (Value(..), Stmt(..))
import Middle.Analyzer.Error
import Middle.Analyzer.Internal
import Middle.Table
import Middle.Typing.Scheme


default (Int, Double)


-- http://dev.stephendiehl.com/fun/006_hindley_milner.html#worked-examples


class Inferable a where
    infer :: TypeEnv -> a -> Analyzer (Subst, Type)


instance Inferable Value where
    infer _ (IntLit _ p) = do
        updatePos p
        return (nullSubst, intType)
    infer _ (FloatLit _ p) = do
        updatePos p
        return (nullSubst, floatType)
    infer _ (DoubleLit _ p) = do
        updatePos p
        return (nullSubst, doubleType)
    infer _ (CharLit _ p) = do
        updatePos p
        return (nullSubst, charType)
    infer _ (StringLit _ p) = do
        updatePos p
        return (nullSubst, stringType)
    infer env (VarVal var) = do
        updatePosVar var
        lookupEnv env var
    infer env (Application val vals) = applyArgs env val vals
    infer env (CtorCall name []) = lookupEnv env name
    infer env (CtorCall name (arg:args)) = do
        updatePosVar name
        (s1, t1) <- lookupEnv env name
        (s2, t2) <- applyArgs env arg args
        s3 <- unify t1 t2
        return (s3 <|> s2 <|> s1, Applied [t2, t1])
    infer env (Lambda ps body) = do
        env' <- pushParams ps env
        -- tv1 <- fresh
        -- (bS, bT) <- foldM (\(s1, t1) stmt -> do
        --     (s2, t2) <- infer env' stmt
        --     s3 <- unify t1 t2
        --     return (s3 <|> s2 <|> s1, apply s3 t2)
        --     ) (nullSubst, tv1) body
        (bS, bT) <- infer env' body
        tv <- fresh
        return (bS, apply bS (Applied [tv, bT]))
    infer env (Tuple arr) = do
        (sub, types) <- foldM (\(s1, types) val -> do
            (s2, typ) <- infer env val
            return (s2 <|> s1, (typ:types))
            ) (nullSubst, []) arr
        return (sub, apply sub (tupleOf (reverse types)))
    infer env (Array arr) = do
        tv <- fresh
        (sub, typ) <- foldM (\(s1, t1) val -> do
            (s2, t2) <- infer env val
            s3 <- unify t1 t2
            return (s3 <|> s2 <|> s1, t1)
            ) (nullSubst, tv) arr
        return (sub, apply sub (arrayOf typ))
    infer env (StmtVal stmt) = infer env stmt
    infer _ (Hole p) = do
        updatePos p
        tv <- fresh
        return (nullSubst, tv)


-- TODO: account for when there is no return
-- statement (bodies have no type)
instance Inferable Stmt where
    infer env (IfElse cond tb fb) = do
        (cS, cT) <- infer env cond
        (tS, tT) <- infer env tb
        (fS, fT) <- infer env fb
        cS' <- unify cT boolType
        bS' <- unify tT fT
        return (compose [bS',cS',fS,tS,cS], apply bS' tT)
    infer env (Loop init' cond iter body) = do
        (inS, _inT) <- infer env init'
        (cS, cT) <- infer env cond
        (itS, _itT) <- infer env iter
        (bS, bT) <- infer env body
        cS' <- unify cT boolType
        return (compose [cS',bS,itS,cS,inS], apply bS bT)
    infer env (Match val cases) = do
        tv <- fresh
        (vS, vT) <- infer env val
        foldM (\(s, t) (ptrn, body) -> do
            (pS, pT) <- infer env ptrn
            (bS, bT) <- infer env body
            vpS <- unify vT pT
            bS' <- unify t bT
            return (compose [bS',vpS,bS,pS,s], bT)
            ) (vS, tv) cases
    infer env (NewVar _mut _name (TypeDecl _ typ) val) = do
        (vS, vT) <- infer env val
        sub <- unify typ vT
        return (sub <|> vS, apply sub typ)
    infer env (Reassignment _name val) = infer env val
    infer env (Return val) = infer env val
    infer env (ValStmt val) = infer env val
    infer _ Break = do
        tv <- fresh
        return (nullSubst, tv)
    infer _ Continue = do
        tv <- fresh
        return (nullSubst, tv)
    infer _ NullStmt = do
        tv <- fresh
        return (nullSubst, tv)

instance Inferable [Stmt] where
    infer _ [] = undefined
    infer env [stmt] = infer env stmt
    infer env (stmt:stmts) = do
        (s1, t1) <- infer env stmt
        (s2, t2) <- infer env stmts
        s3 <- unify t1 t2
        return (s3 <|> s2 <|> s1, apply s3 t2)

lookupEnv :: TypeEnv -> Var -> Analyzer (Subst, Type)
lookupEnv env var = case lookup var env of
    Nothing -> throwUndefined var
    Just scheme -> do
        typ <- instantiate scheme
        return (nullSubst, typ)

unify :: Type -> Type -> Analyzer Subst
unify t1@(Applied ts1) t2@(Applied ts2) = go ts1 ts2
    where
        go (t1':ts1') (t2':ts2') = do
            s1 <- unify t1' t2'
            s2 <- go ts1' ts2'
            return (s1 <|> s2)
        go [] [] = return nullSubst
        go _ _ = throw (UnifyError t1 t2)
unify (Param name []) typ = bind name typ
unify typ (Param name []) = bind name typ
unify (Type nm1 []) (Type nm2 [])
    | nm1 == nm2 = return nullSubst
unify t1 t2 = throw (UnifyError t1 t2)

bind :: Var -> Type -> Analyzer Subst
bind var typ@(Param nm [])
    | var == nm = return nullSubst
    | otherwise = throw (BindError var typ)
bind var typ
    | occurs var typ = throw (InfiniteType var typ)
    | otherwise = return $! singleton var typ

instantiate :: Scheme -> Analyzer Type
instantiate (Forall vars typ) = do
    vars' <- mapM (const fresh) vars
    let s = fromList (zip vars vars')
    return $! apply s typ

generalize :: TypeEnv -> Type -> Scheme
generalize env typ = Forall vars typ
    where
        vars = S.toList (ftv typ `S.difference` ftv env)

applyArgs :: (Inferable a, Inferable b) =>
    TypeEnv -> a -> [b] -> Analyzer (Subst, Type)
applyArgs env a [] = infer env a
applyArgs env a (b:bs) = do
    (s1, t1) <- infer env b
    -- TODO: wont work (vals is a list)
    (s2, t2) <- applyArgs (apply s1 env) a bs
    tv <- fresh
    s3 <- unify (apply s2 t1) (Applied [t2, tv])
    return (compose [s3, s2, s1], apply s3 tv)

-- pushes parameters to the type-environment
pushParams :: [Var] -> TypeEnv -> Analyzer TypeEnv
pushParams [] !env = return env
pushParams (par:pars) env = do
    tv <- fresh
    let env' = extend par (Forall [] tv) env
    pushParams pars env'

newTypeEnv :: Analyzer TypeEnv
newTypeEnv = do
    tbl <- tblTypes <$!> getTable
    mapWithKeyM (\s dt -> do
        let Kind k = dtKind dt
        types <- replicateM (fromIntegral k) fresh
        let name = Var s (dtPos dt)
            typ = Type name types
        return (Forall [] typ)
        ) tbl

inferNew :: Inferable a => a -> Analyzer (Subst, Type)
inferNew a = do
    env <- newTypeEnv
    infer env a
