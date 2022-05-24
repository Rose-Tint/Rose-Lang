module Utils.Path (

) where


infixr 6 :/
data Path
    = String :/ Path
    | Endpoint String
    deriving (Eq, Ord)


infixr 8 //
(//) :: Path -> String -> Path
(root :/ path) // str = root :/ path // str

fromFilePath :: FilePath -> Path
fromFilePath "" = Endpoint ""
fromFilePath ('/':path) = fromFilePath path
fromFilePath ('\\':path) = fromFilePath path
fromFilePath path =
    let (pre, post) = break (`elem` "\\/")
    in pre :/ fromFilePath post

toFilePath :: Path -> FilePath
toFilePath (root :/ path) =
    root ++ "/" ++ toFilePath path
