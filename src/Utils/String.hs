module Utils.String (
    mReadInt,
    readInt,
    areSimilar,
    similarity,
) where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')


mReadInt :: Integral n => Int -> String -> Maybe n
{-# INLINE mReadInt #-}
mReadInt base str = case takeWhile isDigit str of
    [] -> Nothing
    str' -> Just (foldl' (\ !n ch ->
        n * base' + fromIntegral (digitToInt ch))
        0 str')
    where
        !base' = fromIntegral base

readInt :: Integral n => Int -> String -> n
{-# INLINE readInt #-}
readInt base = foldl' (\ !n ch ->
    n * base' + fromIntegral (digitToInt ch))
    0 . takeWhile isDigit
    where
        !base' = fromIntegral base

similarity :: String -> String -> Int
{-# INLINABLE similarity #-}
similarity [] s = length s
similarity s [] = length s
similarity (lc:lcs) (rc:rcs) =
    fromEnum (lc /= rc) + similarity lcs rcs

areSimilar :: String -> String -> Bool
{-# INLINE areSimilar #-}
areSimilar s1 s2 = similarity s1 s2 <=
    min 2 (max (length s1) (length s2))
