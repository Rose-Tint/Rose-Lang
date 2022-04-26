module Parser.Components.Terms (
    term,
    literal,
) where

import Data.Array (listArray)
import Data.Maybe (catMaybes)
import Text.Parsec

import Parser.Components.Identifiers
import Parser.Components.Terms.Literals
import Parser.Components.Internal.LangDef (
    parens, brackets,
    resOper,
    commaSep, commaSep1,
    )
import Parser.Data (Parser, Value(..))


-- = literal
-- | lambda
-- | func-call
-- | "(", term, ")"
term :: Parser Value
term = choice [
        parens term,
        literal,
        ctorCall,
        funcCall,
        lambda
    ] <?> "term"

literal :: Parser Value
literal = choice [
    intLit,
    floatLit,
    charLit,
    stringLit,
    arrayLit,
    tupleLit
    ] <?> "literal"

-- (for now, lambdas will be very limited due to
-- requiring statement)
-- = {small-ident}, "=>", term;
lambda :: Parser Value
lambda = (do
    params <- many smallIdent
    resOper "=>"
    body <- term
    return (Lambda params body)
    ) <?> "lambda"

-- = term, infix-ident, term
-- | infix-ident, term
-- | term, infix-ident;
infixCall :: Parser Value
infixCall = choice [
    try $ do
        lhs <- Just <$> term
        op <- VarVal <$> infixIdent
        rhs <- optionMaybe term
        let args = catMaybes [lhs, rhs]
        return (Application op args),
    do  lhs <- optionMaybe term
        op <- VarVal <$> infixIdent
        rhs <- Just <$> term
        let args = catMaybes [lhs, rhs]
        return (Application op args)
    ] <?> "infix call"

-- = infix-call
-- | small-ident, {term}
-- | "(", operator, ")", {term}
-- | "(", func-call, ")", {term}
-- | "(", lambda, ")", {term}
funcCall :: Parser Value
funcCall = choice [
        try infixCall,
        do  clr <- prefixCaller
            args <- many term
            if null args then
                return clr
            else
                return (Application clr args)
    ] <?> "function call"
    where
        prefixCaller = choice [
                VarVal <$> smallIdent,
                parens (choice [
                        try lambda,
                        try (VarVal <$> operator),
                        funcCall
                    ])
            ]

-- = big-ident, {term}
ctorCall :: Parser Value
ctorCall = (do
    name <- bigIdent
    args <- many term
    return (CtorCall name args)
    ) <?> "constructor call"




-- -- = prefix-ident | "(", lambda, func-app ")";
-- prefixCaller :: Parser Value
-- prefixCaller = (VarVal <$> smallIdent)
--     <|> parens (choice [
--         VarVal <$> operator,
--         try lambda,
--         funcApp
--     <?> "prefixing caller"

-- = prefix-caller, {term},
-- | big-ident, {term}
-- | infix-call;
-- funcApp :: Parser Value
-- funcApp = funcCall <|> ctor
--     <?> "function application"
--     where
--         funcCall = try infixCall <|> prefixCall

-- prefixCall :: Parser Value
-- prefixCall = (do
--     caller <- prefixCaller
--     args <- many term
--     if null args then
--         return caller
--     else
--         return (Application caller args)
--     ) <?> "function call"

-- = "[", {term}, "]";
arrayLit :: Parser Value
arrayLit = (do
    arr <- brackets (commaSep term)
    return (Array (listArray (0, length arr) arr))
    ) <?> "array literal"

-- = "(", term, ",", term, { ",", term }, ")";
tupleLit :: Parser Value
tupleLit = (do
    tup <- parens (commaSep1 term)
    return (Tuple (listArray (0, length tup) tup))
    ) <?> "tuple literal"
