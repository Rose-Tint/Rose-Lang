module Parser (
    module Parser.Imports,
    ParseResults(..),
    parser,
) where

import Text.Parsec

import Parser.Imports
import Parser.Data
import Parser.Parser (parser)


default (Int, Double)


data ParseResults = ParseResults {
        prImports :: [Import],
        prParseTree :: [Expr],
        prPragmas :: [Pragma]
    }            
