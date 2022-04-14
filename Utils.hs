module Utils (
    pathToDir, pathToModule,
    modToPath, modToDir,
    indentAllUsing, indentUsing,
    ord2,
    similarity, areSimilar,
) where


default (Int, Double)


pathToModule :: String -> String
pathToModule [] = []
pathToModule ".th" = []
-- for unix
pathToModule ('/':rest) = ('.':pathToModule rest)
-- for windows
pathToModule ('\\':rest) = ('.':pathToModule rest)
pathToModule (ch:chs) = (ch:pathToModule chs)


-- |Turns a filepath (to a module) or a module
-- name into a filepath as a directory by
-- replacing ".th" with '/'
pathToDir :: FilePath -> FilePath
pathToDir [] = "/"
pathToDir ".th" = "/"
pathToDir (c:cs) = (c:pathToDir cs)


modToPath :: String -> FilePath
modToPath [] = ".th"
modToPath ('.':rest) = ('/':modToPath rest)
modToPath (ch:rest) = (ch:modToPath rest)


modToDir :: FilePath -> String
modToDir [] = "/"
modToDir ('.':rest) = ('/':modToDir rest)
modToDir (ch:rest) = (ch:modToDir rest)


indentAllUsing :: (a -> String) -> [a] -> String
{-# INLINE indentAllUsing #-}
indentAllUsing f = concat . fmap (indentUsing f)


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
