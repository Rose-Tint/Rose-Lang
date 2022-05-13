module Typing.Inferable (
    infer,
    inferStmt,
    inferBody,
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


class Inferable a where
    infer :: TypeEnv -> a -> Infer (Subst, Type)


instance Inferable Literal where
    infer _ IntLit{} = return (nullSubst, intType)
    infer _ FloatLit{} = return (nullSubst, floatType)
    infer _ DoubleLit{} = return (nullSubst, doubleType)
    infer _ CharLit{} = return (nullSubst, charType)
    infer _ StringLit{} = return (nullSubst, stringType)

instance Inferable Value where
    infer env (Literal lit) = infer env lit
    infer env (VarVal name) = searchEnv name env
    infer env (Application val vals) =
        applyArgs env val vals
    infer env (CtorCall name []) =
        searchEnv name env
    infer env (CtorCall name (arg:args)) = do
        (s1, t1) <- searchEnv name env
        (s2, t2) <- applyArgs env arg args
        s3 <- unify t1 t2
        return (s3 <|> s2 <|> s1, t2 :-> t1)
    infer env (Lambda ps body) = do
        env' <- pushParams ps env
        (bS, bT) <- infer env' body
        tv <- fresh
        return (bS, apply bS (tv :-> bT))
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
    infer env (IfElseVal cond tr fa) = do
        (cS, cT) <- infer env cond
        (tS, tT) <- infer env tr
        (fS, fT) <- infer env fa
        cS' <- unify cT boolType
        bS' <- unify tT fT
        return (compose [bS',cS',fS,tS,cS], apply bS' tT)
    infer env (MatchVal val cases) = do
        tv <- fresh
        (vS, vT) <- infer env val
        foldM (\(prS, prT) (ptrn, body) -> do
            (ptS, ptT) <- infer env ptrn
            (bS, bT) <- infer env body
            vpS <- unify vT ptT
            pbS <- unify prT bT
            return (compose [pbS, vpS, bS, ptS, prS], bT)
            ) (vS, tv) cases

instance Inferable Pattern where
    infer env (Param name) = searchEnv name env
    infer _ Hole{} = do
        tv <- fresh
        return (nullSubst, tv)
    infer env (CtorPtrn name []) =
        searchEnv name env
    infer env (CtorPtrn name (arg:args)) = do
        (s1, t1) <- searchEnv name env
        (s2, t2) <- applyPtrns env arg args
        s3 <- unify t1 t2
        return (s3 <|> s2 <|> s1, t2 :-> t1)
    infer env (TuplePtrn args) = do
        (sub, types_) <- foldM (\(s1, types) arg -> do
            (s2, typ) <- infer env arg
            return (s2 <|> s1, (typ:types))
            ) (nullSubst, []) args
        return (sub, TupleType (reverse types_))
    infer env (LitPtrn lit) = infer env lit
    infer env (OrPtrn p1 p2) = do
        (s1, t1) <- infer env p1
        (s2, t2) <- infer env p2
        s3 <- unify t1 t2
        return (s3 <|> s2 <|> s1, apply s3 t1)


-- | Returns `Right (s, t)` only if it can guarantee
-- that it will return (i.e. all cases guarantee a
-- return in a `Match`). If no such guarantee can be
-- made, it will return `Left s`.
inferStmt :: TypeEnv -> Stmt -> Infer (Either Subst (Subst, Type))
inferStmt env (IfElse cond tb fb) = do
    (cS, cT) <- infer env cond
    cS' <- unify boolType cT
    mtb <- foldM (\b stmt -> do
        i <- inferStmt env stmt
        mergeStmtInfs b i
        ) (Left (cS' <|> cS)) tb
    mfb <- foldM (\b stmt -> do
        i <- inferStmt env stmt
        mergeStmtInfs b i
        ) mtb fb
    mergeStmtInfs mtb mfb
    -- case (mtb, mfb) of
    --     (Just (tS, tT), Just (fS, fT)) ->
    --         let sub = compose [cS',fS,tS,cS]
    --         in return (Right (sub, apply sub tT))
    --     _ -> return (Left cS')
inferStmt env (Loop init' cond iter body) = do
    inS <- either id fst <$>
        inferStmt env init'
    cS <- inferCond
    itS <- either id fst <$>
        inferStmt env iter
    bS <- either id fst <$>
        inferBody env body
    let sub = compose [bS,itS,cS,inS]
    return (Left sub)
    where
        -- `inferStmt` would return `Nothing` for
        -- a `ValStmt`
        inferCond = case cond of
            ValStmt val -> do
                (cS, cT) <- infer env val
                sub <- unify cT boolType
                return (sub <|> cS)
            _ -> throw $ OtherError
                "invalid condition expression"
inferStmt env (Match val cases) = do
    tv <- fresh
    (vS, vT) <- infer env val
    foldM (\prev (Case ptrn body) -> case prev of
        Left sub1 -> do
            (pS, pT) <- infer env ptrn
            sub2 <- either id fst <$> inferBody env body
            vpS <- unify vT pT
            return (Left (compose [vpS,sub2,pS,sub1]))
        Right (sub, typ) -> do
            (pS, pT) <- infer env ptrn
            vpS <- unify vT pT
            eith <- inferBody env body
            case eith of
                Left bS ->
                    let sub' = compose [bS, vpS, pS, sub]
                    in return (Left sub')
                Right (bS, bT) -> do
                    sub' <- unify typ bT
                    let sub'' = compose [sub', bS, vpS, pS, sub]
                    return (Right (sub'', bT))
        ) (Right (vS, tv)) cases
inferStmt env (NewVar _mut _name (TypeDecl _ typ) val) = do
    (vS, vT) <- infer env val
    sub <- unify typ vT
    return (Right (sub <|> vS, apply sub typ))
inferStmt env (Reassignment _ val) = do
    (s, _) <- infer env val
    return (Left s)
inferStmt env (Return val) = Right <$> infer env val
inferStmt env (ValStmt val) = do
    (s, _) <- infer env val
    return (Left s)
inferStmt _ _ = return (Left nullSubst)

inferBody :: TypeEnv -> Body -> Infer (Either Subst (Subst, Type))
inferBody env body = do
    tv <- fresh
    foldM (\b stmt -> do
        i <- inferStmt env stmt
        mergeStmtInfs b i
        ) (Right (nullSubst, tv)) body

-- if both bodies guarantee a return, then this can
-- as well. otherwise it cannot be guaranteed.
mergeStmtInfs ::
    Either Subst (Subst, Type) ->
    Either Subst (Subst, Type) ->
    Infer (Either Subst (Subst, Type))
-- mergeStmtInfs (Left s1) (Left s2) = return
--     (Left <$> (s1 <|> s2))
-- mergeStmtInfs (Left s1) (Right (s2, _)) = return
--     (Left <$> (s1 <|> s2))
-- mergeStmtInfs (Right (s1, _)) (Left s2) = return
--     (Left <$> (s1 <|> s2))
mergeStmtInfs (Right (s1, t1)) (Right (s2, t2)) = do
    s3 <- unify t1 t2
    return (Right (s1 <|> s2 <|> s3, t1))
mergeStmtInfs i1 i2 = return
    (Left (either id fst i1 <|> either id fst i2))

pushParams :: [Var] -> TypeEnv -> Infer TypeEnv
pushParams [] env = return env
pushParams (par:pars) env = do
    tv <- fresh
    let env' = extend par (Forall [] tv) env
    pushParams pars env'

applyPtrns :: TypeEnv -> Pattern -> [Pattern] -> Infer (Subst, Type)
applyPtrns env val [] = infer env val
applyPtrns env v1 (v2:vs) = do
    (s1, t1) <- infer env v1
    let env' = apply s1 env
    (s2, t2) <- applyPtrns env' v2 vs
    tv <- fresh
    s3 <- unify (apply s2 t1) (t2 :-> tv)
    return (s3 <|> s2 <|> s1, apply s3 tv)

applyArgs :: TypeEnv -> Value -> [Value] -> Infer (Subst, Type)
applyArgs env val [] = infer env val
applyArgs env v1 (v2:vs) = do
    (s1, t1) <- infer env v1
    let env' = apply s1 env
    (s2, t2) <- applyArgs env' v2 vs
    tv <- fresh
    s3 <- unify (apply s2 t1) (t2 :-> tv)
    return (s3 <|> s2 <|> s1, apply s3 tv)
