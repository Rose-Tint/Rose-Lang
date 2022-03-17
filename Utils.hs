{-# LANGUAGE BangPatterns #-}

module Utils where


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


hamming :: String -> String -> Int
-- {-# INLINE hamming #-}
hamming [] _ = 0 :: Int
hamming _ [] = 0 :: Int
hamming (lc:lcs) (rc:rcs) =
    fromEnum (lc /= rc) + hamming lcs rcs


areSimilar :: String -> String -> Bool
-- {-# INLINABLE areSimilar #-}
areSimilar s1 s2 = hamming s1 s2 <= 2


modPathToRelDir :: FilePath -> FilePath
modPathToRelDir [] = "/"
modPathToRelDir ".th" = "/"
modPathToRelDir (c:cs) = (c:modPathToRelDir cs)


foreachM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
-- {-# INLINE foreachM_ #-}
foreachM_ l f = foldr (\a _ -> f a >> return ()) (return ()) l


-- strict function composition
infix 1 .!
(.!) :: (b -> c) -> (a -> b) -> (a -> c)
bcf .! abf = (\ !b -> bcf $! b) . (\ !a -> abf $! a)
