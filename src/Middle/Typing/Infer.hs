module Middle.Typing.Infer (
    Infer,
    fresh,
    throw,
    throwUndefined,
) where

import Control.Monad (replicateM)

import Middle.Analyzer.Error


newtype IState = IState { freshIndex :: Int }

newtype Infer a = Inf {
    unInf :: forall b. State
        -> (IState -> a -> b)
        -> (Error -> b)
        -> b
    }


instance Functor Infer where
    fmap f (Inf i) = Inf $
        \s okay _ -> okay s . f

instance Applicative Infer where
    pure a = Inf $ \s okay _ -> okay a s
    f <*> x = do
        f' <- f
        x' <- x
        return $! f' x'

instance Monad Infer where
    Inf inf >>= f = Inf $ \s okay err  ->
        let okay' x s' = runA (f x) s' okay err
        in inf s okay' err


modifyInfState :: (IState -> IState) -> Infer IState
modifyInfState f = Inf $ \s okay _ ->
    let s' = f s in okay s' s'

fresh :: Infer Type
fresh = do
    state <- modifyState $ \s ->
        s { freshIndex = freshIndex s + 1 }
    let i = freshIndex state
    return (Param (prim (letters !! i)) [])
    where
        letters = [1..]
            >>= flip replicateM ['a'..'z']

throw :: Error -> Infer a
throw e = Inf $ \_ _ err -> err e

throwUndefined :: Var -> Analyzer a
throwUndefined sym = throw $ Undefined sym []
