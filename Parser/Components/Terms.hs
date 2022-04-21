module Parser.Components.Terms (
    term,
    infixCall, prefixCall, ctorCall,
    pattern,
) where

import Control.Monad ((<$!>))
import Data.Array (listArray)
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Data.Maybe (catMaybes)
import Text.Parsec
import Data.Text (Text)

import Parser.Data
import Parser.Keywords
import Parser.LangDef
import Parser.Pragmas


default (Int, Double)


term :: Parser Value
term = choice [
        intLit, fltLit, chrLit, strLit, arrLit,
        try infixCall,
        try prefixCall,
        ctorCall,
        parens term
    ] <?> "term"

infixCall :: Parser Value
infixCall = (do
    lhs <- term
    op <- operator <|>
        between (resOper "`") (resOper "`") smallIden
    rhs <- term
    return (FuncCall op [lhs, rhs])
    ) <?> "operator call"

prefixCall :: Parser Value
prefixCall = (do
    name <- funcName
    args <- arguments
    return (FuncCall name args)
    ) <?> "function call"

ctorCall :: Parser Value
ctorCall = (do
    name <- bigIden
    args <- arguments
    return (CtorCall name args)
    ) <?> "constructor call"

-- functionCall :: Parser Value
-- functionCall = try infixCall <|> prefixCall

hole :: Parser Value
hole = do
    pos <- getPosition
    keyword "_"
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + 1)
    return (Hole pos')

ctorPattern :: Parser Value
ctorPattern = do
    name <- bigIden
    as <- many pattern

pattern :: Parser Value
pattern = choice [
        hole,
        smallIdent,
        brackets (literal <|> ctorPattern)
    ] <?> "pattern"

funcName :: Parser Variable
funcName = smallIden <|> parens operator

arguments :: Parser [Value]
arguments = many term
