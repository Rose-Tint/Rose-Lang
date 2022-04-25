{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (
    module Color,
    Pretty(..),
    sepsT, seps, sepsD,
    indentLnsT, indentLns, indentLnsD,
    indentCatLnsT, indentCatLns, indentCatLnsD,
    (.<),(.^),(.>),
    (-|),(|-|),(|-),
    (+|),(|+|),(|+),
    (*|),(|*|),(|*),
) where

import Data.Int
import Data.List (intercalate)
import Data.Text (Text, unpack)

import Color


default (Int, Double)


data Align a
    = AL Int Char a
    | AC Int Char a
    | AR Int Char a

-- |Required: terse | pretty
class Pretty a where
    terse :: a -> String
    pretty :: a -> String
    detailed :: a -> String
    terse = pretty
    detailed = pretty


infixr 5 `sepsT`, `seps`, `sepsD`
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

infix 7 .<, .^, .>
(.<), (.^), (.>) :: Int -> a -> Align a
n .< a = AL n ' ' a
n .^ a = AC n ' ' a
n .> a = AR n ' ' a

infixr 1 -|, +|, *|
(-|), (+|), (*|) :: Pretty a => String -> a -> String
(-|) s = (s ++) . terse
(+|) s = (s ++) . pretty
(*|) s = (s ++) . detailed

infixr 1 |-, |+, |*
(|-), (|+), (|*) :: Pretty a => a -> String -> String
(|-) = (++) . terse
(|+) = (++) . pretty
(|*) = (++) . detailed

infixr 1 |-|, |+|, |*|
(|-|), (|+|), (|*|) :: (Pretty a, Pretty b) => a -> b -> String
a |-| b = terse a ++ terse b
a |+| b = pretty a ++ pretty b
a |*| b = detailed a ++ detailed b

alignL :: Int -> Char -> String -> String
alignL n pc [] = replicate n pc
alignL n _ str = drop n str
-- | n <= 0 = []
-- | otherwise = c:alignL (n - 1) pc cs

alignC :: Int -> Char -> String -> String
alignC n pc str = case compare padLen 0 of
    LT -> take n str
    EQ -> str
    GT -> pad ++ str ++ (
        if diff `mod` 2 == 0 then
            pad
        else
            pc:pad
        )
    where
        pad = replicate padLen pc
        diff = n - length str
        padLen = diff `div` 2

alignR :: Int -> Char -> String -> String
alignR n pc str
    | n < strLen = take (strLen - n) str
    | n == 0 = []
    | otherwise = str ++ replicate n pc
    where
        strLen = length str


instance Pretty Int8 where
    pretty = show

instance Pretty Int where
    pretty = show

instance Pretty Char where
    pretty = (:[])

instance Pretty String where
    pretty = id

instance Pretty Text where
    pretty = unpack

instance Pretty a => Pretty (Align a) where
    terse (AL n c a) = alignL n c (terse a)
    terse (AC n c a) = alignC n c (terse a)
    terse (AR n c a) = alignR n c (terse a)
    pretty (AL n c a) = alignL n c (pretty a)
    pretty (AC n c a) = alignC n c (pretty a)
    pretty (AR n c a) = alignR n c (pretty a)
    detailed (AL n c a) = alignL n c (detailed a)
    detailed (AC n c a) = alignC n c (detailed a)
    detailed (AR n c a) = alignR n c (detailed a)
        

instance Pretty a => Pretty (Maybe a) where
    terse Nothing = ""
    terse (Just a) = terse a
    pretty Nothing = ""
    pretty (Just a) = pretty a
    detailed Nothing = ""
    detailed (Just a) = detailed a
