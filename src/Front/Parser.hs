module Front.Parser (
    module Front.Parser.Imports,
    module Front.Parser.Data,
    Module(..),
    parse,
) where

import Front.Parser.Imports
import Front.Parser.Data
import Front.Parser.Lexer
import Front.Parser.Parser


parse :: String -> Either String Module
parse str = runAlex str rose
