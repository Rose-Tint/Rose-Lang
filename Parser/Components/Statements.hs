module Parser.Components.Statements (
    match, ifElse,
    body,
    bodyAssignment,
) where

import Text.Parsec

import Common.Typing.Type (Type(NoType))
import Common.Var
import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef
import Parser.Components.Patterns
import Parser.Components.Specifiers
import Parser.Components.Terms
import Parser.Components.Types
import Parser.Data (
    Parser,
    Stmt(..),
    Body,
    Value,
    Pattern,
    Mutability,
    )


-- = if-else | match | return | loop
-- | new-var, ";" | reassignment, ";" | body;
statement :: Parser Stmt
statement = choice [
        returnE,
        ifElse,
        loop,
        match,
        newVar <* semi,
        try reassignment <* semi,
        ValStmt <$> term
    ] <?> "statement"

-- = "{", {statement}, "}";
body :: Parser [Stmt]
body = braces (many statement)
    <?> "body"

-- = "=", statement | body;
bodyAssignment :: Parser [Stmt]
bodyAssignment = choice [
        do  resOper "="
            stmt <- statement
            return [stmt],
        body
    ] <?> "body assignment"

-- = statement | body
stmtBody :: Parser [Stmt]
stmtBody = ((:[]) <$> statement) <|> body

-- = "if","(", term, ")", stmt-body, ["else", stmt-body - if-else];
ifElse :: Parser Stmt
ifElse = (do
    keyword "if"
    clause <- parens term <?> "if-clause"
    trueBody <- stmtBody
    falseBody <- option [] (keyword "else" >> stmtBody)
    return (IfElse clause trueBody falseBody)
    ) <?> "if statement"

-- = "return", (term, ";" | match | if-else)
returnE :: Parser Stmt
returnE = (do
    keyword "return"
    val <- term
    semi
    return (Return val)
    ) <?> "return statement"

-- = pattern, body-assignment;
matchCase :: Parser (Pattern, Body)
matchCase = do
    ptrn <- pattern <?> "match pattern"
    bdy <- bodyAssignment <?> "pattern body"
    return (ptrn, bdy)

-- = "match","(", term, ")","{", case, {case}, "}";
match :: Parser Stmt
match = (do
    keyword "match"
    val <- term <?> "match clause"
    cases <- many1 matchCase
    return (Match val cases)
    ) <?> "match statement"

-- (will this work? if the initial)
-- = "(", [ term, ";" ], term, [ ";", term ], ")";
loopClause :: Parser (Maybe Stmt, Value, Maybe Stmt)
loopClause = parens (do
    cInit <- optionMaybe (statement <* semi)
    condClause <- term
    cIter <- optionMaybe (semi *> statement)
    return (cInit, condClause, cIter)
    ) <?> "loop clause"

-- = "loop", loop-clause, stmt-body;
loop :: Parser Stmt
loop = (do
    keyword "loop"
    (cInit, cond, cIter) <- loopClause
    bdy <- stmtBody
    return (Loop cInit cond cIter bdy)
    ) <?> "loop statement"

-- = "let", mutability, prefix-ident, [type-decl];
varDeclSeq :: Parser (Mutability, Var, Type)
varDeclSeq = do
    keyword "let"
    mut <- option Pure mutability
    name <- smallIdent
    typ <- option NoType (angles ttype)
    return (mut, name, typ)

-- = "=", term;
assignment :: Parser Value
assignment = resOper "=" >>
    (term <?> "assignment body")
    <?> "assignment"

-- = var-decl-seq, assignment;
newVar :: Parser Stmt
newVar = (do
    (mut, name, typ) <- varDeclSeq
    val <- assignment
    return (NewVar mut typ name val)
    ) <?> "new variable declaration"

-- = small-ident, assignment;
reassignment :: Parser Stmt
reassignment = (do
    name <- smallIdent
    val <- assignment
    return (Reassignment name val)
    ) <?> "variable reassignment"
