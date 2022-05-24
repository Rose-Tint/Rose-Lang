module Common.Module (
    ModName(..),
    modEndpoint,
    pathToMod,
    modToDir,
    modToFile,
    strToMod,
    normMod,
) where

import Text.Pretty


infixr 9 :.
data ModName
    = String :. ModName
    | End String
    deriving (Eq, Ord)


modEndpoint :: ModName -> String
modEndpoint (_ :. m) = modEndpoint m
modEndpoint (End s) = s

pathToMod :: FilePath -> ModName
pathToMod "" = End ""
pathToMod ".th" = End ""
pathToMod ('/':path) = pathToMod path
pathToMod ('\\':path) = pathToMod path
pathToMod path = case break (`elem` "\\/") path of
    (pre, []) -> End (takeWhile (/= '.') pre)
    ([], post) -> pathToMod post
    (pre, post) -> case pathToMod post of
        End "" -> End pre
        (s :. End "") -> pre :. End s
        name -> pre :. name

normMod :: ModName -> ModName
normMod (s :. End "") = End s
normMod (s :. m) = s :. normMod m
normMod m = m

modToDir :: ModName -> FilePath
modToDir (root :. rest) = root++"/"++modToDir rest
modToDir (End name) = name++"/"

modToFile :: ModName -> String -> FilePath
modToFile (root :. rest) ext = root++"/"++modToFile rest ext
modToFile (End name) ext = name++"/"++name++ext

strToMod :: String -> ModName
strToMod "" = End ""
strToMod ('.':str) = strToMod str
strToMod str = case break (== '.') str of
    (pre, []) -> End pre
    ([], post) -> strToMod post
    (pre, post) -> case strToMod post of
        End "" -> End pre
        (s :. End "") -> pre :. End s
        name -> pre :. name


instance Pretty ModName where
    pretty (End s) = s
    pretty (root :. rest) = root|+"."+|rest
