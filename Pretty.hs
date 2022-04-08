{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (
    module Color,
    Pretty(..),
    seps, sepsD, sepsE,
    prettyPrint,
    hPrettyPrint,
    (+|),(|+|),(|+),(+\),
    (*|),(|*|),(|*),(*\),
    (|+*|), (|*+|),
) where

import Control.Monad ((<$!>))
import Data.List (intercalate)
import System.IO (Handle, hPutStrLn)

import Color


default (Int, Double)



class Pretty a where
    pretty :: a -> String
    -- |Like pretty, but a more detailed version. Defaults
    -- to pretty
    detailed :: a -> String
    detailed = pretty
    -- |Like detailed, but even more detailed. Defaults
    -- to detailed
    exhaustive :: a -> String
    exhaustive = detailed

    default pretty :: (Show a) => a -> String
    pretty = show


seps, sepsD, sepsE :: (Pretty a) => String -> [a] -> String
seps sep as = intercalate sep (pretty <$!> as)
sepsD sep as = intercalate sep (detailed <$!> as)
sepsE sep as = intercalate sep (exhaustive <$!> as)

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

hPrettyPrint :: Pretty a => Handle -> a -> IO ()
hPrettyPrint hdl = hPutStrLn hdl . pretty

infixr 1 +|
infixr 1 *|
(+|), (*|) :: String -> String -> String
(+|) = (++)
(*|) = (++)

infixr 1 |+
infixr 1 |*
(|+), (|*) :: Pretty a => a -> String -> String
(|+) = (++) . pretty
(|*) = (++) . detailed

infixr 1 |+|
infixr 1 |*|
infixr 1 |+*|
infixr 1 |*+|
(|+|), (|*|), (|+*|), (|*+|)
    :: (Pretty a, Pretty b) => a -> b -> String
a |+| b = pretty a ++ pretty b
a |*| b = detailed a ++ detailed b
a |+*| b = pretty a ++ detailed b
a |*+| b = detailed a ++ pretty b

infixr 2 +\
infixr 2 *\
(+\), (*\) :: (Pretty a, Pretty b) => a -> b -> String
a +\ b = pretty a ++ ('\n':pretty b)
a *\ b = detailed a ++ ('\n':detailed b)



instance Pretty Char where
    pretty = (:[])

instance Pretty String where
    pretty = id
