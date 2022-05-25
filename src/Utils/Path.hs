module Utils.Path (

) where






infixr 6 :/
data Path
    = String :/ Path
    | File String String
    deriving (Eq, Ord)


infixr 8 //
(//) :: Path -> String -> Path
(root :/ path) // str = root :/ path // str

fromFilePath :: FilePath -> Path
fromFilePath "" = rootDir
fromFilePath str = case parseHome str of
    Nothing -> go str
    Just str' -> go str'
    where
        go path = case break isDirSep path of
            (path', []) -> mkFile path'
            ([], []) -> rootDir
            (dirName, path') -> dirName :/ go path'
        parseHome ('~':rest) = Just (dropWhile isDirSep rest)
        parseHome (c:cs) | isDirSep c = Just (dropWhile isDirSep cs)
        parseHome _ = Nothing

mkFile :: String -> Path
mkFile str =
    let (revExt, revName) = break (== '.') (reverse str)
    in File (reverse revName) (reverse revExt)

isValidPathChar :: Char -> Bool
isValidPathChar c = isAlphaNum c || c `elem` "_- ."

isDirSep :: Char -> Bool
isDirSep '/' = True
isDirSep '\\' = True
isDirSep _ = False

toFilePath :: Path -> FilePath
toFilePath (root :/ path) =
    root ++ "/" ++ toFilePath path
