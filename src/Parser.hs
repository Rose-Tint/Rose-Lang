module Parser (
    module Parser.Imports,
    module Parser.Data,
    Module(..),
    parse,
) where

import Parser.Imports
import Parser.Data
import Parser.Lexer
import Parser.Parser


parse :: String -> Either String Module
parse str = runAlex str rose
