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
        intLit, fltLit, chrLit, strLit,
        arrLit, try tupLit,
        try lambda
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
    name <- smallIden <|> parens operator
    args <- many term
    return (FuncCall name args)
    ) <?> "function call"

ctorCall :: Parser Value
ctorCall = (do
    name <- bigIdent
    args <- many term
    return (CtorCall name args)
    ) <?> "constructor call"

-- for now, lambdas will be very limited due to
-- requiring statement
lambda :: Parser Value
lambda = (do
    params <- many smallIdent
    resOper "=>"
    body <- ExprVal <$> term
    ) <?> "lambda"

hole :: Parser Value
hole = (do
    pos <- getPosition
    keyword "_"
    let pos' = SourcePos
            (prim (sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + 1)
    return (Hole pos')
    ) <?> "hole"

ctorPattern :: Parser Value
ctorPattern = do
    name <- bigIden
    as <- many pattern

arrLit :: Parser Value
arrLit = (do
    pos <- getPosition
    arr <- brackets (commaSep term)
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (prim (sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            end
    return (Array (listArray (0, length arr) arr) pos')
    ) <?> "array literal"

tupLit :: Parser Value
tupLit = (do
    pos <- getPosition
    tup <- parens (commaSep1 term)
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (prim (sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            end
    return (Tuple (listArray (0, length tup) tup) pos')
    ) <?> "tuple literal"

tuplePattern :: Parser Value
tuplePattern = (do
    pos <- getPosition
    tup <- parens (commaSep1 pattern)
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (prim $! sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            end
    return (Tuple (listArray (0, length tup) tup) pos')
    ) <?> "tuple pattern"

pattern :: Parser Value
pattern = choice [
        hole,
        smallIdent,
        brackets (commaSep1 nonWild)
    ] <?> "pattern"
    where
        nonWild = choice [
            literal,
            tuplePattern,
            ctorPattern
        ]
