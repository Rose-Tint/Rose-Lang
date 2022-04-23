module Parser.Components.Terms (
    term,
    literal,
) where

import Data.Array (listArray)
import Text.Parsec (
    many, choice, try, (<|>),
    (<?>),
    )

import Parser.Components.Identifiers
import Parser.Components.Terms.Literals
import Parser.Components.Internal.LangDef (
    parens, brackets,
    resOper,
    commaSep, commaSep1,
    )
import Parser.Data (Parser, Value(..))


-- = literal | func-app | lambda | "(", term, ")";
term :: Parser Value
term = choice [
        literal,
        try lambda,
        funcApp,
        try tupleLit,
        parens term
    ]

literal :: Parser Value
literal = choice [
    intLit,
    floatLit,
    charLit,
    stringLit,
    arrayLit
    -- `tupleLit` is excluded bc of the
    -- parentheses. find it in `term`.
    ] <?> "literal"

-- = term, infix-ident, term
-- | infix-ident, term
-- | term, infix-ident;
infixCall :: Parser Value
infixCall = (do
    lhs <- term
    op <- VarVal <$> infixIdent
    rhs <- term
    return (Application op [lhs, rhs])
    ) <?> "infix call"

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

-- = prefix-ident | ("(", lambda, func-app ")");
prefixCaller :: Parser Value
prefixCaller = (VarVal <$> smallIdent) <|> parens (choice [
        VarVal <$> operator,
        try lambda,
        funcApp
    ]) <?> "prefixing caller"

-- = prefix-caller, {term},
-- | big-ident, {term}
-- | infix-call;
funcApp :: Parser Value
funcApp = funcCall <|> ctor
    where
        funcCall = try infixCall <|> prefixCall
            <?> "function call"
        prefixCall = do
            caller <- prefixCaller
            args <- many term
            return (Application caller args)
        ctor = (do
            name <- bigIdent
            args <- many term
            return (CtorCall name args)
            ) <?> "constructor"

{-
@arrayLit@ and @tupleLit@ must be in this
module because they require @term@.
-}

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
