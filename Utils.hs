module Utils (
    pathToModule,
    indentUsing,
    indentAllUsing,
) where


pathToModule :: String -> String
pathToModule [] = []
pathToModule ".th" = []
-- for unix
pathToModule ('/':rest) = ('.':pathToModule rest)
-- for windows
pathToModule ('\\':rest) = ('.':pathToModule rest)
pathToModule (ch:chs) = (ch:pathToModule chs)


indentAllUsing :: (a -> String) -> [a] -> String
indentAllUsing f as = concat $ fmap (indentUsing f) as


indentUsing :: (a -> String) -> a -> String
indentUsing f a = unlines $ fmap
    ((++) "    |   ")
    (lines $ f a)
