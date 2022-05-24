module Parser (
    parseFile,
) where

import AST (ParseTree)
import Builder
import Parser.Parser (runAlex, rose)
import Text.Pretty


parse :: String -> Either String ParseTree
parse str = runAlex str rose

parseFile :: Builder ParseTree
parseFile = do
    name <- gets moduleName
    debug ("Parsing   ["+|name|+"]\n")
    src <- gets sourceCode
    case parse src of
        Left msg -> fatal $ Red|+|name|+|msg|+"\n"
        Right parseTree -> do
            traceFile "Parse-Tree.txt" parseTree
            return parseTree
