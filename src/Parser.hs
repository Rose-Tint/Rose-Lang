module Parser (
    parseFile,
) where

import Common.Module
import Builder
import Parser.Parser (runAlex, rose)
import Text.Pretty


parse :: String -> Either String Module
parse str = runAlex str rose

parseFile :: Builder Module
parseFile = do
    name <- gets moduleName
    debug ("Parsing   ["+|name|+"]\n")
    src <- gets sourceCode
    case parse src of
        Left msg -> fatal $ Red|+|name|+|msg|+"\n"
        Right parseTree -> do
            trace "Parse-Tree.txt" parseTree
            return parseTree
