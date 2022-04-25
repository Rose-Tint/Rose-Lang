module Parser.Components.Datatypes (
    dataDef,
    typeAlias
) where

import Text.Parsec (
    choice, many, option,
    sepBy,
    try, (<?>)
    )

import Common.Typing
import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef
import Parser.Components.Specifiers
import Parser.Components.Types
import Parser.Data


-- = small-ident, type-decl;
fieldDecl :: Parser Field
fieldDecl = (do
    name <- smallIdent
    TypeDecl _ typ <- typeDeclNoCtx
    return (Field name typ)
    ) <?> "field declaration"

-- = big-ident, {type}
-- | big-ident, [ "{", field-decl,
--     { ",", field-decl }, [","], "}" ];
ctorDef :: Parser Ctor
ctorDef = (do
    vis <- visibility
    name <- bigIdent
    choice [
        SumType vis name <$> many ttype,
        Record vis name <$>
            braces (commaSepEnd fieldDecl)
        ]
    ) <?> "constructor definition"

-- = visib, "data", big-ident,
--     "=", ctor-def, { "|", ctor-def };
dataDef :: Parser Expr
dataDef = do
    vis <- visibility
    keyword "data"
    name <- bigIdent
    pars <- many smallIdent
    ctors <- option [] $ do
        resOper "="
        ctor1 <- ctorDef
        ctors <- try ctorDef
            `sepBy` resOper "|"
        return (ctor1:ctors)
    return (DataDef vis name pars ctors)

-- = visib, "using", type, "=", type;
typeAlias :: Parser Expr
typeAlias = do
    vis <- visibility
    keyword "using"
    alias <- ttype
    resOper "="
    typ <- ttype
    return (TypeAlias vis alias typ)
