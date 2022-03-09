{-# LANGUAGE Rank2Types #-}

module Analyzer.Analyzer where

import Analyzer.State
import SymbolTable



data Analyzer a
    = Analyzer {
        unAnalyzer :: forall b .
            State
            -> (a -> State -> b)
            -> b
    }



getTable :: Analyzer SymbolTable
getTable = Analyzer $ \s run -> run (stTable s) s


setTable :: SymbolTable -> Analyzer ()
setTable tbl = Analyzer $ \s run ->
    run () (s { stTable = tbl })


modifyTable :: (SymbolTable -> SymbolTable) -> Analyzer ()
modifyTable f = Analyzer $ \s run ->
    run () (s { stTable = f (stTable s) })



instance Functor Analyzer where
    fmap f a = Analyzer $ \s run ->
        let run' a' s' = run (f a') s' in
        unAnalyzer a s run'


instance Applicative Analyzer where
    pure a = Analyzer $ \s run -> run a s
    fa <*> xa = do
        f <- fa
        x <- xa
        return (f x)


instance Monad Analyzer where
    return = pure
    m >>= f = Analyzer $ \s run ->
        let run' x s' = unAnalyzer (f x) s' run in
        unAnalyzer m s run'
