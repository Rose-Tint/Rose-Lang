{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (
    Color(..),
    Pretty(..),
    color, uncolor, colored, reset,
    sepsT, seps, sepsD,
    indentLnsT, indentLns, indentLnsD,
    indentCatLnsT, indentCatLns, indentCatLnsD,
    printf, processString,
    (.<),(.^),(.>),
    (-|),(|-|),(|-),
    (+|),(|+|),(|+),
    (*|),(|*|),(|*),
) where

import Text.Printf hiding (printf)
import qualified Text.Printf as P

import Data.Char (isDigit)
import Data.Int
import Data.List (intercalate)
import Utils.String


default (Int, Double)


data Align a
    = AL Int Char a
    | AC Int Char a
    | AR Int Char a

data Color
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Purple
    | Cyan
    | White
    | Reset


class Pretty a where
    terse :: a -> String
    pretty :: a -> String
    detailed :: a -> String
    terse = pretty
    detailed = pretty

    default pretty :: (Show a) => a -> String
    pretty = show


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
        if even diff then
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
    | n < strLen = drop (strLen - n) str
    | n == 0 = []
    | otherwise = replicate n pc ++ str
    where
        strLen = length str

color :: String -> String
color [] = pretty Reset
color [ch] = [ch]
color ('\\':('$':rest)) = ('$':color rest)
color ('$':(ch:rest)) = clrStr ++ color rest
    where
        clrStr = case ch of
            'B' -> pretty Black
            'r' -> pretty Red
            'y' -> pretty Yellow
            'g' -> pretty Green
            'b' -> pretty Blue
            'p' -> pretty Purple
            'c' -> pretty Cyan
            'w' -> pretty White
            'R' -> pretty Reset
            _   -> ['$', ch]
color (c:cs) = (c:color cs)

colored :: String -> Color -> String
{-# INLINE colored #-}
colored str clr = pretty clr ++ str ++ pretty Reset

reset :: String -> String
{-# INLINE reset #-}
reset str = pretty Reset ++ str

printf :: (PrintfType a) => String -> a
printf = P.printf . color

uncolor :: String -> String
uncolor [] = []
uncolor ('\027':'[':'0':'m':str) = uncolor str
uncolor ('\027':'[':'3':_:'m':str) = uncolor str
uncolor (c:cs) = (c:uncolor cs)

-- |Does some formatting, such as repeating characters
processString :: String -> String
processString [] = []
processString (ch:chs) = case ch of
    '\\' -> case chs of
        [] -> []
        (ch':chs') -> (ch':processString chs')
    '#' -> case span isDigit chs of
        (numStr, (ch':chs')) ->
            let count = unsafeReadInt numStr
            in replicate count ch' ++ processString chs'
        _ -> ('#':processString chs)
    '$' -> case chs of
        [] -> ('$':processString chs)
        (ch':_) -> case ch' of
            'B' -> pretty Black
            'r' -> pretty Red
            'y' -> pretty Yellow
            'g' -> pretty Green
            'b' -> pretty Blue
            'p' -> pretty Purple
            'c' -> pretty Cyan
            'w' -> pretty White
            'R' -> pretty Reset
            _   -> ('$':processString chs)
    _ -> (ch:processString chs)


instance Pretty Color where
    pretty Black  = "\027[30m"
    pretty Red    = "\027[31m"
    pretty Green  = "\027[32m"
    pretty Yellow = "\027[33m"
    pretty Blue   = "\027[34m"
    pretty Purple = "\027[35m"
    pretty Cyan   = "\027[36m"
    pretty White  = "\027[37m"
    pretty Reset  = "\027[0m"

instance PrintfArg Color where
    formatArg = formatString . pretty

instance Pretty Int8
instance Pretty Int

instance Pretty Char where
    pretty = (:[])

instance Pretty String where
    pretty = id


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
