module Parser.Components.Types (
    ttype,
    typeDecl,
) where

import Control.Monad ((<$!>))
import Data.Array (listArray)
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Data.Maybe (catMaybes)
import Text.Parsec
import Data.Text (Text)

import Parser.Data
import Parser.Keywords
import Parser.LangDef (brackets)
import Parser.Pragmas


default (Int, Double)


ttype :: Parser Type
ttype = choice [
        namedType,
        arrayType,
        try tupleType,
        -- try unitType,
        funcType
    ] <?> "type"
    where
        arrayType = Type (Prim "[]") <$> (:[]) <$> brackets ttype
        tupleType = Type (Prim ",") <$> parens (commaSep1 ttype)
        namedType = Type <$> identifier <*> many ttype
        -- unitType = resOper "()" >> TerminalType (Prim "()") []
        funcType = Applied <$> parens (ttype `sepBy1` resOper "->")

constraint :: Parser Constraint
constraint = do
    con <- bigIdent
    typeArgs <- many1 smallIdent
    return (Constraint con typeArgs)

ctxDeclSeq :: Parser Context
ctxDeclSeq = commaSep1 constraint <* resOper ":"

typeDecl :: Parser TypeDecl
typeDecl = angles $ do
    ctx <- option [] ctxDeclSeq
    typ <- ttype `sepBy1` resOper "->"
    return (TypeDecl ctx typ)
