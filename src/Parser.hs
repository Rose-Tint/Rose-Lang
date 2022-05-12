module Parser (
    parseFile,
) where

import Common.Module
import Parser.Base
import Parser.Lexer
import Parser.Parser
import Pretty


parse :: String -> Either String Module
parse str = runAlex str rose

parseFile :: BuilderIO Module
parseFile = do
    name <- getModule
    debug ("Parsing   ["+|name|+"]\n")
    src <- getSource
    case parse src of
        Left msg -> fatal $ Red|+|name|+|msg|+"\n"
        Right parseTree -> do
            trace "Parse-Tree.txt" parseTree
            return parseTree
