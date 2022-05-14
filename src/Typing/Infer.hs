{-# LANGUAGE Rank2Types #-}

module Typing.Infer (
    Infer,
    runInfer,
    getEnv,
    local,
    extendEnv,
    applyEnv,
    refresh,
    fresh,
    throw,
    -- recover,
    throwUndefined,
    lookupEnv,
    searchEnv,
    findEnv,
    instantiate,
    generalize
) where

import Prelude hiding (lookup)

import Control.Monad (replicateM)
-- import Control.Monad.Trans.RWS.CPS
import Control.Monad.Trans.State
import qualified Data.Set as S

import Analysis.Error
import Common.SrcPos
import Common.Var
import Data.VarMap
import Typing.Scheme
import Typing.Substitution
import Typing.Type
import Typing.TypeEnv


data InferState = InfState {
    stFreshIndex :: Int,
    stErrors :: [ErrInfo],
    stEnv :: TypeEnv
    }

type Infer a = State InferState a


runInfer :: Infer a -> (a, [ErrInfo], TypeEnv)
runInfer inf =
    let (a, InfState _ errs env) =
            runState inf (InfState 0 [] emptyEnv)
    in (a, reverse errs, env)

fresh :: Infer Type
fresh = do
    modify $ \s -> s { stFreshIndex = stFreshIndex s + 1 }
    i <- gets stFreshIndex
    return (TypeVar (prim (letters !! i)))
    where
        letters = [1..]
            >>= flip replicateM ['a'..'z']

local :: (TypeEnv -> TypeEnv) -> Infer a -> Infer a
local f = withState $ \s -> s { stEnv= f (stEnv s) }

getEnv :: Infer TypeEnv
getEnv = gets stEnv

extendEnv ::  Var -> Scheme -> Infer ()
extendEnv name scheme = modify $ \s ->
    s { stEnv = extend name scheme (stEnv s) }

applyEnv :: Subst -> Infer ()
applyEnv sub = modify $ \s ->
    s { stEnv = apply sub (stEnv s) }

refresh :: Infer ()
refresh = modify $ \s -> s { stFreshIndex = 0 }

-- | Adds the error to the error list
throw :: Error -> Infer ()
throw err = do
    let ei = ErrInfo (getPos err) (Right err)
    modify $ \s -> s { stErrors = (ei:stErrors s) }

-- recover :: Infer a -> Infer (Maybe a)
-- recover inf = lift catchE (Just <$> inf) handle
--     where
--         handle _err = return Nothing

throwUndefined :: Var -> Infer ()
throwUndefined sym = throw (Undefined sym [])

lookupEnv :: Var -> Infer (Maybe (Subst, Type))
lookupEnv var = do
    env <- getEnv
    case lookup var env of
        Nothing -> return Nothing
        Just scheme -> do
            typ <- instantiate scheme
            return (Just (nullSubst, typ))

searchEnv :: Var -> Infer (Subst, Type)
searchEnv var = do
    env <- getEnv
    case lookup var env of
        Nothing -> do
            tv <- fresh
            return (singleton var tv, tv)
        Just scheme -> do
            typ <- instantiate scheme
            return (nullSubst, typ)

findEnv :: Var -> Infer (Subst, Type)
findEnv var = do
    env <- getEnv
    case lookup var env of
        Nothing -> do
            throwUndefined var
            tv <- fresh
            return (nullSubst, tv)
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
