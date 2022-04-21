module Parser.Components.Statements (
    body,
    bodyAssignment
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


-- = if-else | match | return | loop
-- | new-var, ";" | reassignment, ";" | body;
statement :: Parser Expr
statement = choice [
        ifElse,
        match,
        returnE,
        loop,
        newVar <* semi,
        reassignment <* semi
    ] <?> "statement"

-- = "{", {statement}, "}";
body :: Parser Expr
body = Body <$> braces (many statement)

-- = "=", statement | body;
bodyAssignment :: Parser Expr
bodyAssignment = (resOper "=" >> statement)

-- = statement | body
stmtBody :: Parser [Expr]
stmtBody = ((:[]) <$> statement) <|> body

-- = "if","(", term, ")", stmt-body, ["else", stmt-body - if-else];
ifElse :: Parser Expr
ifElse = do
    keyword "if"
    clause <- parens term <?> "if-clause"
    trueBody <- stmtBody
    falseBody <- option [] (keyword "else" >> stmtBody)
    return (IfElse clause trueBody falseBody)

-- = "return", (term, ";" | match | if-else)
returnE :: Parser Expr
returnE = do
    keyword "return"
    choice [
            term <* semi,
            ExprVal <$> match,
            ExprVal <$> ifElse
        ]

-- = pattern, body-assignment;
matchCase :: Parser (Pattern, Body)
matchCase = do
    ptrn <- pattern
    bdy <- bodyAssignment <?> "pattern body"
    return (ptrn, bdy)

-- = "match","(", term, ")","{", case, {case}, "}";
match :: Parser Expr
match = (Match <$> val) <*> many1 matchCase

-- (will this work? if the initial)
-- = "(", [ term, ";" ], term, [ ";", term ], ")";
loopClause :: Parser (Maybe Value, Value, Maybe Value)
loopClause = parens (do
    cInit <- optionMaybe (term <* semi)
    condClause <- term
    cIter <- optionMaybe (semi <* term)
    return (cInit, condClause, cIter)
    )

-- = "loop", loop-clause, stmt-body;
loop :: Parser Expr
loop = do
    keyword "loop"
    (cInit, cond, cIter) <- loopClause
    bdy <- stmtBody
    return (Loop cInit cond cIter bdy);

-- = "let", mutability, prefix-ident, [type-decl];
varDeclSeq :: Parser (Mutability, Var, TypeDecl)
verDeclSeq = do
    keyword "let"
    mut <- mutability
    name <- smallIdent
    typ <- option NoType typeDecl
    return (mut, name, typ)

-- = "=", term;
assignment :: Parser Value
assignment = resOper "=" >> term

-- = var-decl-seq, assignment;
newVar :: Parser Expr
newVar = do
    (mut, name, typ) <- varDeclSeq
    val <- assignment
    return (NewVar mut typ name val)

-- = small-ident, assignment;
reassignment :: Parser Expr
reassignment = do
    name <- smallIdent
    val <- assignment
    return (Reassignment name val)
