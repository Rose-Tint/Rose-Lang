{-# LANGUAGE Rank2Types #-}

module Typing.Infer (
    local, -- from RWS
    ask, -- from RWS
    throwE, -- from Except

    Infer,
    runInfer,
    fresh,
    -- throw,
    -- recover,
    -- throwUndefined,
    searchEnv,
    instantiate,
    generalize,
    constrain,
) where

import Prelude hiding (lookup)

import Control.Monad (replicateM)
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS
import qualified Data.Set as S

import Analysis.Error
import Common.Var
import Data.VarMap
import Typing.Scheme
import Typing.Substitution
import Typing.Type
import Typing.TypeEnv


type Constraint = (Type, Type)

data InfState = InfState {
    -- allowBreak :: Bool,
    -- purity :: Purity,
    -- table :: Table,
    -- stPos :: SrcPos,
    freshIdx :: Int
    }

type Infer a = RWST
    TypeEnv -- | `Reader` input
    [Constraint] -- | `Writer` output
     -- no `Except` to cleanly allow multiple errors per file
    InfState
    (Except Error)
    a


runInfer :: TypeEnv -> Infer a -> Either Error ([Constraint], a)
runInfer env inf =
    case runExcept $  evalRWST inf env (InfState 0) of
        Left err -> Left err
        Right (a, cons) -> Right (cons, a)

fresh :: Infer Type
fresh = do
    modify $ \s -> s { freshIdx = freshIdx s + 1 }
    i <- gets freshIdx
    return (TypeVar (prim (letters !! i)))
    where
        letters = [1..]
            >>= flip replicateM ['a'..'z']

-- throwUndefined :: Var -> Infer ()
-- throwUndefined sym = throw (Undefined sym [])

searchEnv :: Var -> Infer Type
searchEnv var = do
    env <- ask
    case lookup var env of
        Nothing -> do
            tv <- fresh
            return tv
        Just scheme -> do
            typ <- instantiate scheme
            return typ

instantiate :: Scheme -> Infer Type
instantiate (Forall vars typ) = do
    vars' <- mapM (const fresh) vars
    let sub = fromList (zip vars vars')
    return $! apply sub typ

generalize :: Type -> Infer Scheme
generalize typ = do
    env <- ask
    let vars = S.toList (ftv typ `S.difference` ftv env)
    return (Forall vars typ)

constrain :: Type -> Type -> Infer ()
constrain t1 t2 = tell [(t1, t2)]
