module Utils.String (
    areSimilar,
    similarity,
) where


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
