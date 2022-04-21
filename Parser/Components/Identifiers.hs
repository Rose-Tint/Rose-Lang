module Parser.Components.Identifiers (
    bigIdent, smallIdent,
    operator,
    infixIdent, prefixIdent,
    identifier
) where

import Data.Function (on)
import Data.Ord (comparing)
import Text.Parsec
import qualified Text.Parsec.Token as T (
    identifier,
    operator
    )

import Parser.Data
import Parser.LangDef


default (Int, Double)


qualifier :: Parser String
qualifier = concat <$> many $ try $ do
    lookAhead upper
    qual <- T.identifier tokenP
    resOper "."
    return $! qual ++ "."

bigIdent :: Parser Variable
bigIdent = (do
    pos <- getPosition
    qual <- qualifier
    lookAhead upper
    name <- T.identifier tokenP
    let !name' = qual ++ name
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name')
    return (Var name' pos')
    ) <?> "big identifier"

smallIdent :: Parser Variable
smallIdent = (do
    pos <- getPosition
    qual <- qualifier
    lookAhead lower
    name <- T.identifier tokenP
    let !name' = qual ++ name
        pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name')
    return (Var name' pos')
    ) <?> "small identifier"

operator :: Parser Variable
operator = (do
    pos <- getPosition
    qual <- qualifier
    op <- T.operator tokenP
    let !op' = qual ++ op
        pos' = SourcePos
            (Module Export (Prim $ sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length op')
    return (Var op' pos'))
    <?> "operator"

infixIdent :: Parser Variable
infixIdent = operator <|> (resOper "`" *> smallIdent <* resOper "`")
    <?> "infix identifier"

prefixIdent :: Parser Variable
prefixIdent = smallIdent <|> parens infixIdent
    <?> "prefix identifier"

identifier :: Parser Variable
identifier = smallIdent <|> bigIdent <|> parens operator
    <?> "identifier"
