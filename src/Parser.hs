module Parser (
    parseFile,
) where

import Common.Module
import Builder
import Parser.Lexer
import Parser.Parser
import Text.Pretty


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
