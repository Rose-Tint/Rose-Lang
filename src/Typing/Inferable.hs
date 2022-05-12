module Typing.Inferable (

) where

import Prelude hiding (lookup)

import Control.Monad ()
import qualified Data.Set as S

import Common.Var
import AST.Value
import AST.Stmt
import Analysis.Error
import Analysis
import Data.Table
import Typing.Scheme


class Inferable a where
    infer :: TypeEnv -> a -> Infer (Subst, Type)


inferApply :: Substitutable a =>
    TypeEnv -> a -> Infer Type
inferApply env = uncurry apply . infer env


instance Inferable Value where
    infer _ IntLit{} = return (nullSubst, intType)
    infer _ FloatLit{} = return (nullSubst, floatType)
    infer _ DoubleLit{} = return (nullSubst, doubleType)
    infer _ CharLit{} = return (nullSubst, charType)
    infer _ StringLit{} = return (nullSubst, stringType)
    infer env (VarVal name) = searchEnv name env
    infer env (Application val vals) =
        applyArgs env val vals
    infer env (CtorCall name []) =
        searchEnv name env
    infer env (CtorCall name (arg:args)) = do
        (s1, t1) <- searchEnv env name
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
    -- infer env (IfElse cond tb fb) = do
    --     (cS, cT) <- infer env cond
    --     (tS, tT) <- infer env tb
    --     (fS, fT) <- infer env fb
    --     cS' <- unify cT boolType
    --     bS' <- unify tT fT
    --     return (compose [bS',cS',fS,tS,cS], apply bS' tT)
    -- infer env (Loop init' cond iter body) = do
    --     (inS, _inT) <- infer env init'
    --     (cS, cT) <- infer env cond
    --     (itS, _itT) <- infer env iter
    --     (bS, bT) <- infer env body
    --     cS' <- unify cT boolType
    --     return (compose [cS',bS,itS,cS,inS], apply bS bT)
    -- infer env (Match val cases) = do
    --     tv <- fresh
    --     (vS, vT) <- infer env val
    --     foldM (\(s, t) (ptrn, body) -> do
    --         (pS, pT) <- infer env ptrn
    --         (bS, bT) <- infer env body
    --         vpS <- unify vT pT
    --         bS' <- unify t bT
    --         return (compose [bS',vpS,bS,pS,s], bT)
    --         ) (vS, tv) cases

-- | Returns `Right (s, t)` only if it can guarantee
-- that it will return (i.e. all cases guarantee a
-- return in a `Match`). If no such guarantee can be
-- made, it will return `Left s`.
inferStmt :: TypeEnv -> Stmt
    -> Infer (Either Subst (Subst, Type))
inferStmt env (IfElse cond tb fb) = do
    (cS, cT) <- infer env cond
    cS' <- unify boolType cT
    mtb <- foldM (\b stmt -> do
        i <- inferStmt env stmt
        return (mergeStmtInfs b i)
        ) (Left nullSubst) tb
    mfb <- foldM (\b stmt -> do
        i <- inferStmt env stmt
        return (mergeStmtInfs b i)
        ) mtb fb
    -- if both bodies guarantee a return, then this can
    -- as well. otherwise it cannot be guaranteed.
    case (mtb, mfb) of
        (Just (tS, tT), Just (fS, fT)) ->
            let sub = compose [bS',cS',fS,tS,cS]
            in return (sub, apply bS' tT)
        _ -> return Nothing
inferStmt env (Loop init' cond iter body) = do
    _ <- infer env init'
    (cS, cT) <- inferVS env cond
    (itS, _) <- infer env iter
    (bS, bT) <- infer env body
    cS' <- unify cT boolType
    return (compose [cS',bS,itS,cS,inS], apply bS bT)
    where
        -- `inferStmt` would return `Nothing` for
        -- a `ValStmt`
        inferVS (ValStmt val) =
            Just <$> infer env val
        -- we still want its substitutions, so dont
        -- just return `Nothing`
        inferVS stmt = inferStmt stmt
inferStmt env (Match val cases) = do
    tv <- fresh
    (vS, vT) <- infer env val
    foldM (\(s, t) (ptrn, body) -> do
        (pS, pT) <- infer env ptrn
        (bS, bT) <- infer env body
        vpS <- unify vT pT
        bS' <- unify t bT
        return (compose [bS',vpS,bS,pS,s], bT)
        ) (vS, tv) cases
inferStmt env (NewVar _mut _name (TypeDecl _ typ) val) = do
    (vS, vT) <- infer env val
    sub <- unify typ vT
    return (sub <|> vS, apply sub typ)
inferStmt env (Reassignment _ val) = do
    (s, _) <- infer env val
    return (Left s)
inferStmt env (Return val) = Right <$> infer env val
inferStmt env (ValStmt val) = do
    (s, _) <- infer env val
    return (Left s)
inferStmt _ Break = return (Left nullSubst)
inferStmt _ Continue = return (Left nullSubst)
inferStmt _ NullStmt = return (Left nullSubst)

mergeStmtInfs ::
    Either Subst (Subst, Type) ->
    Either Subst (Subst, Type) ->
    Either Subst (Subst, Type)
mergeStmtInfs i1 i2 = either id snd i1 <|> either id snd i2
