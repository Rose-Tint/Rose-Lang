{-# LANGUAGE Rank2Types #-}

module Typing.Infer (
    Infer,
    fresh,
    throw,
    throwUndefined,
) where

import Control.Monad (replicateM)

import Analysis.Analyzer ()
import Analysis.Error
import Common.Var
import Typing.Type


newtype State = State { freshIndex :: Int }

newtype Infer a = Inf {
    unInf :: forall b. State
        -> (State -> a -> b)
        -> (Error -> b)
        -> b
    }


instance Functor Infer where
    fmap f (Inf inf) = Inf $ \s okay ->
        let okay' s' = okay s' . f
        in inf s okay'

instance Applicative Infer where
    pure a = Inf $ \s okay _ -> okay s a
    f <*> x = do
        f' <- f
        x' <- x
        return $! f' x'

instance Monad Infer where
    Inf inf >>= f = Inf $ \s okay err  ->
        let okay' s' x = unInf (f x) s' okay err
        in inf s okay' err


modifyState :: (State -> State) -> Infer State
modifyState f = Inf $ \s okay _ ->
    let s' = f s in okay s' s'

fresh :: Infer Type
fresh = do
    state <- modifyState $ \s ->
        s { freshIndex = freshIndex s + 1 }
    let i = freshIndex state
    return (TypeVar (prim (letters !! i)))
    where
        letters = [1..]
            >>= flip replicateM ['a'..'z']

throw :: Error -> Infer a
throw e = Inf $ \_ _ err -> err e

throwUndefined :: Var -> Infer a
throwUndefined sym = throw $ Undefined sym []
