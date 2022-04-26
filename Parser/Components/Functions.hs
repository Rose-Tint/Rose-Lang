module Parser.Components.Functions (
    funcDecl,
    funcDef,
) where

import Text.Parsec

import Common.Var
import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef
import Parser.Components.Patterns
import Parser.Components.Specifiers
import Parser.Components.Statements
import Parser.Components.Terms
import Parser.Components.Types
import Parser.Data (
    Parser,
    Stmt(Return),
    Expr(FuncDecl, FuncDef),
    Value,
    )


-- = visibility, purity, prefix-ident, type-decl;
funcDecl :: Parser Expr
funcDecl = (do
    pur <- purity
    vis <- option Extern visibility
    name <- prefixIdent
    typ <- typeDecl
    return (FuncDecl vis pur name typ)
    ) <?> "function declaration"

parameter :: Parser Value
parameter = pattern <?> "function parameter"

-- = pattern, infix-ident, pattern;
infixParamSeq :: Parser (Var, [Value])
infixParamSeq = do
    lhs <- parameter
    op <- infixIdent
    rhs <- parameter
    return (op, [lhs, rhs])

-- = prefix-ident, {pattern};
prefixParamSeq :: Parser (Var, [Value])
prefixParamSeq = do
    func <- prefixIdent
    args <- many parameter
    return (func, args)

funcBody :: Parser [Stmt]
funcBody = choice [
        resOper "=" >> (:[]) <$> choice [
            match,
            ifElse,
            Return <$> term
            ],
        body
    ] <?> "function body"

-- = func-param-seq, body-assignment;
funcDef :: Parser Expr
funcDef = (do
    -- = infix-param-seq | prefix-param-seq
    (name, params) <- try infixParamSeq
        <|> prefixParamSeq
    bdy <- funcBody
    return (FuncDef name params bdy)
    ) <?> "function definition"
