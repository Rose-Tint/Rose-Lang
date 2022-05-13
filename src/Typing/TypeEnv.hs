module Typing.TypeEnv (
    TypeEnv,
    extend,
    restrict,
    emptyEnv,
    lookupEnv,
    searchEnv,
    findEnv,
    instantiate,
    generalize
) where

import Prelude hiding (lookup)

import qualified Data.Set as S

import Common.Var
import Data.VarMap
import Typing.Infer
import Typing.Scheme
import Typing.Substitution
import Typing.Type


type TypeEnv = VarMap Scheme


extend :: Var -> Scheme -> TypeEnv -> TypeEnv
extend = insert

restrict :: Var -> TypeEnv -> TypeEnv
restrict = delete

-- TODO: 'empty' contains primatives
emptyEnv :: TypeEnv
emptyEnv = empty

lookupEnv :: Var -> TypeEnv
    -> Infer (Maybe (Subst, Type))
lookupEnv var env = case lookup var env of
    Nothing -> return Nothing
    Just scheme -> do
        typ <- instantiate scheme
        return (Just (nullSubst, typ))

searchEnv :: Var -> TypeEnv -> Infer (Subst, Type)
searchEnv var env = case lookup var env of
    Nothing -> do
        tv <- fresh
        return (singleton var tv, tv)
    Just scheme -> do
        typ <- instantiate scheme
        return (nullSubst, typ)

findEnv :: Var -> TypeEnv -> Infer (Subst, Type)
findEnv var env = case lookup var env of
    Nothing -> throwUndefined var
    Just scheme -> do
        typ <- instantiate scheme
        return (nullSubst, typ)

instantiate :: Scheme -> Infer Type
instantiate (Forall vars typ) = do
    vars' <- mapM (const fresh) vars
    let sub = fromList (zip vars vars')
    return $! apply sub typ

generalize :: TypeEnv -> Type -> Scheme
generalize env typ = Forall vars typ
    where
        vars = S.toList (ftv typ `S.difference` ftv env)
