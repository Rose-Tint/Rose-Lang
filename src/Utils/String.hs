module Utils.String (
    readInt,
    unsafeReadInt,
    areSimilar,
    similarity,
) where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')


readInt :: Integral n => String -> Maybe n
readInt str = case takeWhile isDigit str of
    [] -> Nothing
    str' -> Just (foldl' (\ !n ch ->
        n * 10 + fromIntegral (digitToInt ch))
        0 str')

unsafeReadInt :: Integral n => String -> n
unsafeReadInt = foldl' (\ !n ch ->
    n * 10 + fromIntegral (digitToInt ch))
    0 . takeWhile isDigit

similarity :: String -> String -> Int
{-# INLINABLE similarity #-}
similarity [] s = length s
similarity s [] = length s
similarity (lc:lcs) (rc:rcs) =
    fromEnum (lc /= rc) + similarity lcs rcs

areSimilar :: String -> String -> Bool
{-# INLINE areSimilar #-}
areSimilar s1 s2 = similarity s1 s2 <=
    min 3 (max (length s1) (length s2))
