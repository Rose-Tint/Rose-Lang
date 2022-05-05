module Utils.Paths (
    modToDir,
    modToPath,
    pathToDir,
    pathToModule
) where


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
