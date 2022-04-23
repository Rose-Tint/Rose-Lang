module Parser.Components.Statements (
    body,
    bodyAssignment
) where

import Text.Parsec (
    choice, many, many1, (<|>),
    option, optionMaybe,
    (<?>),
    )

import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef (
    braces, parens, angles,
    resOper, keyword,
    semi,
    )
import Parser.Components.Patterns
import Parser.Components.Specifiers
import Parser.Components.Terms
import Parser.Components.Types
import Parser.Data (
    Parser,
    Stmt(..),
    Body,
    Value(StmtVal),
    Pattern,
    Var,
    Type(NoType),
    Mutability,
    )


-- = if-else | match | return | loop
-- | new-var, ";" | reassignment, ";" | body;
statement :: Parser Stmt
statement = choice [
        ifElse,
        match,
        returnE,
        loop,
        newVar <* semi,
        reassignment <* semi
    ] <?> "statement"

-- = "{", {statement}, "}";
body :: Parser [Stmt]
body = braces (many statement)

-- = "=", statement | body;
bodyAssignment :: Parser [Stmt]
bodyAssignment = (resOper "=" >> (:[]) <$> statement) <|> body

-- = statement | body
stmtBody :: Parser [Stmt]
stmtBody = ((:[]) <$> statement) <|> body

-- = "if","(", term, ")", stmt-body, ["else", stmt-body - if-else];
ifElse :: Parser Stmt
ifElse = do
    keyword "if"
    clause <- parens term <?> "if-clause"
    trueBody <- stmtBody
    falseBody <- option [] (keyword "else" >> stmtBody)
    return (IfElse clause trueBody falseBody)

-- = "return", (term, ";" | match | if-else)
returnE :: Parser Stmt
returnE = do
    keyword "return"
    val <- choice [
            term <* semi,
            StmtVal <$> match,
            StmtVal <$> ifElse
        ]
    return (Return val)

-- = pattern, body-assignment;
matchCase :: Parser (Pattern, Body)
matchCase = do
    ptrn <- pattern
    bdy <- bodyAssignment <?> "pattern body"
    return (ptrn, bdy)

-- = "match","(", term, ")","{", case, {case}, "}";
match :: Parser Stmt
match = (Match <$> term) <*> many1 matchCase

-- (will this work? if the initial)
-- = "(", [ term, ";" ], term, [ ";", term ], ")";
loopClause :: Parser (Maybe Stmt, Value, Maybe Stmt)
loopClause = parens (do
    cInit <- optionMaybe (statement <* semi)
    condClause <- term
    cIter <- optionMaybe (semi *> statement)
    return (cInit, condClause, cIter)
    )

-- = "loop", loop-clause, stmt-body;
loop :: Parser Stmt
loop = do
    keyword "loop"
    (cInit, cond, cIter) <- loopClause
    bdy <- stmtBody
    return (Loop cInit cond cIter bdy);

-- = "let", mutability, prefix-ident, [type-decl];
varDeclSeq :: Parser (Mutability, Var, Type)
varDeclSeq = do
    keyword "let"
    mut <- mutability
    name <- smallIdent
    typ <- option NoType (angles ttype)
    return (mut, name, typ)

-- = "=", term;
assignment :: Parser Value
assignment = resOper "=" >> term

-- = var-decl-seq, assignment;
newVar :: Parser Stmt
newVar = do
    (mut, name, typ) <- varDeclSeq
    val <- assignment
    return (NewVar mut typ name val)

-- = small-ident, assignment;
reassignment :: Parser Stmt
reassignment = do
    name <- smallIdent
    val <- assignment
    return (Reassignment name val)
