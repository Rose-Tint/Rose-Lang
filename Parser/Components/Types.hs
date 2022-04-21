module Parser.Components.Types (
    Type(..),
    ttype,
    typeDecl,
) where

import Text.Parsec

import Parser.Components.Identifiers (
    identifier,
    bigIdent,
    smallIdent
    )
import Parser.Data (Parser, Type(..))
import Parser.LangDef


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
    args <- many1 smallIdent
    return (Constraint con args)

ctxDeclSeq :: Parser Context
ctxDeclSeq = commaSep1 constraint <* resOper ":"

typeDecl :: Parser TypeDecl
typeDecl = angles $ do
    ctx <- option [] ctxDeclSeq
    typ <- ttype `sepBy1` resOper "->"
    return (TypeDecl ctx typ)
