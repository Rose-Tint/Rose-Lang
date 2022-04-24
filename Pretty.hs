{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (
    module Color,
    Pretty(..),
    prettyPrint,
    hPrettyPrint,
    sepsT, seps, sepsD,
    indentLnsT, indentLns, indentLnsD,
    indentCatLnsT, indentCatLns, indentCatLnsD,
    (-|),(|-|),(|-),
    (+|),(|+|),(|+),
    (*|),(|*|),(|*),
) where

import Data.Int
import Data.List (intercalate)
import System.IO (Handle, hPutStrLn)

import Color


default (Int, Double)


-- |Required: terse | pretty
class Pretty a where
    -- |Like pretty, but terse. Defaults to pretty.
    terse :: a -> String
    terse = pretty
    -- |Returns a human-readable text-representation.
    pretty :: a -> String
    -- |Like pretty, but a more detailed version. Defaults
    -- to pretty
    detailed :: a -> String
    detailed = pretty


prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

hPrettyPrint :: Pretty a => Handle -> a -> IO ()
hPrettyPrint hdl = hPutStrLn hdl . pretty

infixr 2 `sepsT`
infixr 2 `seps`
infixr 2 `sepsD`
sepsT, seps, sepsD :: Pretty a => String -> [a] -> String
sepsT sep as = intercalate sep (terse <$> as)
seps sep as = intercalate sep (pretty <$> as)
sepsD sep as = intercalate sep (detailed <$> as)

indentLnsT, indentLns, indentLnsD :: Pretty a => a -> String
indentLnsT = unlines . fmap ("    "++) . lines . terse
indentLns = unlines . fmap ("    "++) . lines . pretty
indentLnsD = unlines . fmap ("    "++) . lines . detailed

indentCatLnsT, indentCatLns, indentCatLnsD
    :: (Pretty a, Foldable t) => t a -> String
indentCatLnsT = concatMap indentLnsT
indentCatLns = concatMap indentLns
indentCatLnsD = concatMap indentLnsD

infixr 1 -|
infixr 1 +|
infixr 1 *|
(-|), (+|), (*|) :: Pretty a => String -> a -> String
(-|) s = (s ++) . terse
(+|) s = (s ++) . pretty
(*|) s = (s ++) . detailed

infixr 1 |-
infixr 1 |+
infixr 1 |*
(|-), (|+), (|*) :: Pretty a => a -> String -> String
(|-) = (++) . terse
(|+) = (++) . pretty
(|*) = (++) . detailed

infixr 1 |-|
infixr 1 |+|
infixr 1 |*|
(|-|), (|+|), (|*|) :: (Pretty a, Pretty b) => a -> b -> String
a |-| b = terse a ++ terse b
a |+| b = pretty a ++ pretty b
a |*| b = detailed a ++ detailed b


instance Pretty Int8 where
    pretty = show

instance Pretty Int where
    pretty = show

instance Pretty Char where
    pretty = (:[])

instance Pretty String where
    pretty = id

instance Pretty a => Pretty (Maybe a) where
    terse Nothing = ""
    terse (Just a) = terse a
    pretty Nothing = ""
    pretty (Just a) = pretty a
    detailed Nothing = ""
    detailed (Just a) = detailed a
