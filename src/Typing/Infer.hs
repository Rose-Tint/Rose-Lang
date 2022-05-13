{-# LANGUAGE Rank2Types #-}

module Typing.Infer (
    Infer,
    stErrors,
    runInfer,
    fresh,
    throw,
    recoverMaybe,
    recoverOpt,
    recover,
    throwUndefined,
) where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)

import Analysis.Error
import Common.SrcPos
import Common.Var
import Typing.Type


data State = State {
    stFreshIndex :: Int,
    stErrors :: [ErrInfo]
    }

newtype Infer a = Inf {
    unInf :: forall b. State
        -> (State -> a -> b)
        -> (State -> Error -> b)
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


runInfer :: Infer a -> (State -> a -> b) -> (State -> Error -> b) -> b
runInfer (Inf inf) = inf (State 0 [])

modifyState :: (State -> State) -> Infer State
modifyState f = Inf $ \s okay _ ->
    let s' = f s in okay s' s'

fresh :: Infer Type
fresh = do
    state <- modifyState $ \s ->
        s { stFreshIndex = stFreshIndex s + 1 }
    let i = stFreshIndex state
    return (TypeVar (prim (letters !! i)))
    where
        letters = [1..]
            >>= flip replicateM ['a'..'z']

throw :: Error -> Infer a
throw e = Inf $ \s _ err ->
    let ei = case e of
            Undefined name _ -> ErrInfo (varPos name) (Right e)
            Redefinition _orig new -> ErrInfo (varPos new) (Right e)
            BindError name _ -> ErrInfo (varPos name) (Right e)
            MissingReturn name -> ErrInfo (varPos name) (Right e)
            _ -> ErrInfo UnknownPos (Right e)
        s' = s { stErrors = (ei:stErrors s) }
    in err s' e

recoverMaybe :: Infer a -> Infer (Maybe a)
recoverMaybe (Inf inf) = Inf $ \s okay _ ->
    let okay' s' = okay s' . Just
        err' s' _ = okay s' Nothing
    in inf s okay' err'

recoverOpt :: a -> Infer a -> Infer a
recoverOpt def inf = fromMaybe def <$>
    recoverMaybe inf

recover :: Infer a -> Infer ()
recover inf = recoverOpt () (inf >> return ())

throwUndefined :: Var -> Infer a
throwUndefined sym = throw $ Undefined sym []
