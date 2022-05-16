module Utils.String (
    mReadInt,
    readInt,
    areSimilar,
    similarity,
    strToShortBS,
) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')


mReadInt :: Integral n => Int -> String -> Maybe n
mReadInt base str = case takeWhile isDigit str of
    [] -> Nothing
    str' -> Just (foldl' (\ !n ch ->
        n * base' + fromIntegral (digitToInt ch))
        0 str')
    where
        !base' = fromIntegral base

readInt :: Integral n => Int -> String -> n
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
    min 3 (max (length s1) (length s2))

strToShortBS :: String -> ShortByteString
strToShortBS = toShort . pack
