module Parser.Components.Functions (
    funcDecl,
    funcDef,
) where

import Text.Parsec (many, try, (<|>), (<?>))

import Common.Var
import Parser.Components.Identifiers
import Parser.Components.Patterns
import Parser.Components.Specifiers
import Parser.Components.Statements
import Parser.Components.Types
import Parser.Data (
    Parser,
    Expr(FuncDecl, FuncDef),
    Value,
    )


-- = visibility, purity, prefix-ident, type-decl;
funcDecl :: Parser Expr
funcDecl = (do
    vis <- visibility
    pur <- purity
    name <- prefixIdent
    typ <- typeDecl
    return (FuncDecl vis pur name typ)
    ) <?> "function declaration"

-- = pattern, infix-ident, pattern;
infixParamSeq :: Parser (Var, [Value])
infixParamSeq = do
    lhs <- pattern
    op <- infixIdent
    rhs <- pattern
    return (op, [lhs, rhs])

-- = prefix-ident, {pattern};
prefixParamSeq :: Parser (Var, [Value])
prefixParamSeq = do
    func <- prefixIdent
    args <- many pattern
    return (func, args)

-- = infix-param-seq | prefix-param-seq
funcParamSeq :: Parser (Var, [Value])
funcParamSeq = try infixParamSeq <|> prefixParamSeq

-- = func-param-seq, body-assignment;
funcDef :: Parser Expr
funcDef = do
    (name, params) <- funcParamSeq
    bdy <- bodyAssignment
    return (FuncDef name params bdy)
