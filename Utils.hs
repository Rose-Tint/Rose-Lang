{-# LANGUAGE BangPatterns #-}

module Utils where


default (Int, Double)


pathToModule :: String -> String
{-# INLINABLE pathToModule #-}
pathToModule [] = []
pathToModule ".th" = []
-- for unix
pathToModule ('/':rest) = ('.':pathToModule rest)
-- for windows
pathToModule ('\\':rest) = ('.':pathToModule rest)
pathToModule (ch:chs) = (ch:pathToModule chs)


moduleToPath :: String -> String
moduleToPath [] = ".th"
moduleToPath ('.':rest) = ('/':moduleToPath rest)
moduleToPath (ch:rest) = (ch:moduleToPath rest)


indentAllUsing :: (a -> String) -> [a] -> String
{-# INLINE indentAllUsing #-}
indentAllUsing f = concat .! fmap (indentUsing f)


indentUsing :: (a -> String) -> a -> String
{-# INLINE indentUsing #-}
indentUsing f a = unlines $ fmap
    ((++) "    |   ")
    (lines $ f a)


ord2 :: (Ord a) => (a, a) -> (a, a)
{-# INLINE ord2 #-}
ord2 xy@(x, y)
    | x <= y = xy
    | otherwise = (y, x)


clamp :: (Ord a) => a -> a -> a -> a
{-# INLINE clamp #-}
clamp a mn mx 
    | a < mn' = mn'
    | a > mx' = mx'
    | otherwise = a
    where
        (mn', mx') = ord2 (mn, mx)


similarity :: String -> String -> Int
-- {-# INLINE similarity #-}
similarity [] s = length s
similarity s [] = length s
similarity (lc:lcs) (rc:rcs) =
    fromEnum (lc /= rc) + similarity lcs rcs


areSimilar :: String -> String -> Bool
{-# INLINABLE areSimilar #-}
areSimilar s1 s2 = similarity s1 s2 <=
    min 3 (max (length s1) (length s2))


modPathToRelDir :: FilePath -> FilePath
modPathToRelDir [] = "/"
modPathToRelDir ".th" = "/"
modPathToRelDir (c:cs) = (c:modPathToRelDir cs)


-- strict function composition
infix 1 .!
(.!) :: (b -> c) -> (a -> b) -> (a -> c)
{-# INLINE (.!) #-}
bcf .! abf = (\ !b -> bcf $! b) . (\ !a -> abf $! a)
