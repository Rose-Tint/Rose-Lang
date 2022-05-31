module Common.Module (
    ModName(..),
    Import,
    modEndpoint,
    pathToMod,
    modToDir,
    modToContFile,
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

type Import = ModName


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

-- | Converts a @`ModName`@ to a file-path such that the
-- file-name is the 'endpoint' of the module name with
-- the given extension, and the parent directory of said
-- file is the 'endpoint' as well.
--
-- Examples:
-- 1) "Std."
--
-- >>> modToContFile (strToMod "Std.Data.Maybe" ".abc")
-- "Std/Data/Maybe/Maybe.abc"
--
-- (un-abbreviated: module to contained file)
modToContFile :: ModName -> String -> FilePath
modToContFile (root :. rest) ext = root++"/"++modToContFile rest ext
modToContFile (End name) ext = name++"/"++name++ext

modToFile :: ModName -> String -> FilePath
modToFile (root :. rest) ext = root++"/"++modToFile rest ext
modToFile (End name) ext = name++ext

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
