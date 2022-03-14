module Utils where


pathToModule :: String -> String
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
indentAllUsing f as = concat $ fmap (indentUsing f) as


indentUsing :: (a -> String) -> a -> String
indentUsing f a = unlines $ fmap
    ((++) "    |   ")
    (lines $ f a)


hamming :: String -> String -> Int
hamming [] _ = 0 :: Int
hamming _ [] = 0 :: Int
hamming (lc:lcs) (rc:rcs) =
    fromEnum (lc /= rc) + hamming lcs rcs


areSimilar :: String -> String -> Bool
areSimilar s1 s2 = hamming s1 s2 <= 2


modPathToRelDir :: FilePath -> FilePath
modPathToRelDir [] = "/"
modPathToRelDir ".th" = "/"
modPathToRelDir (c:cs) = (c:modPathToRelDir cs)


foreachM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
foreachM_ l f = foldr (\a _ -> f a >> return ()) (return ()) l
