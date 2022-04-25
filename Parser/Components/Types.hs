module Parser.Components.Types (
    constraint,
    ttype,
    typeDecl, typeDeclNoCtx,
) where

import Text.Parsec (
    many, many1, sepBy1,
    option, choice, (<|>),
    try, (<?>),
    )

import Common.Typing
import Common.Var
import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef (
    angles, brackets, parens,
    commaSep1,
    resOper,
    )
import Parser.Data (Parser)


-- (ignore mutability for now)
-- = [mutability], "[", type, "]"
-- | [mutability], big-ident, {type}
-- | [mutability], small-ident, {type}
-- | [mutability], "(", type, ",", type, { ",", type } ")"
-- | "(", type, { "->", type }, ")";
ttype :: Parser Type
ttype = choice [
        namedType,
        arrayType',
        try tupleType,
        -- try unitType,
        funcType
    ] <?> "type"
    where
        arrayType' = arrayOf <$> brackets ttype
        tupleType = Type (prim ",") <$> parens (commaSep1 ttype)
        namedType = Type <$> (bigIdent <|> smallIdent) <*> many ttype
        -- unitType = resOper "()" >> TerminalType (Prim "()") []
        funcType = Applied <$> parens (ttype `sepBy1` resOper "->")

-- = big-ident, small-ident, {small-ident};
constraint :: Parser Constraint
constraint = do
    con <- bigIdent
    args <- many1 smallIdent
    return (Constraint con args)

-- = constraint, { ",", constraint }, ":";
ctxDeclSeq :: Parser Context
ctxDeclSeq = commaSep1 constraint <* resOper ":"

-- = "<", [ctx-decl-seq], type, { "->", type }, ">";
typeDecl :: Parser TypeDecl
typeDecl = angles $ do
    ctx <- option [] ctxDeclSeq
    typ <- Applied <$> ttype `sepBy1` resOper "->"
    return (TypeDecl ctx typ)

-- = "<", type, { "->", type }, ">";
typeDeclNoCtx :: Parser TypeDecl
typeDeclNoCtx = angles $ do
    typ <- Applied <$> ttype `sepBy1` resOper "->"
    return (TypeDecl [] typ)
