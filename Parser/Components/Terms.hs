module Parser.Components.Terms (
    term,
) where


-- = literal | func-app | lambda | "(", term, ")";
term :: Parser Value
term = choice [
        literal,
        try lambda,
        funcApp,
        try tupleLit,
        parens term
    ]

-- = term, infix-ident, term
-- | infix-ident, term
-- | term, infix-ident;
infixCall :: Parser Value
infixCall = (do
    lhs <- term
    op <- operator <|>
        between (resOper "`") (resOper "`") smallIden
    rhs <- term
    return (FuncCall op [lhs, rhs])
    ) <?> "infix call"

-- (for now, lambdas will be very limited due to
-- requiring statement)
-- = {small-ident}, "=>", term;
lambda :: Parser Value
lambda = (do
    params <- many smallIdent
    resOper "=>"
    body <- StmtVal <$> term
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
    -- pos <- getPosition
    arr <- brackets (commaSep term)
    -- end <- sourceColumn <$> getPosition
    -- let pos' = (mkPos pos) { srcEnd = end }
    return (Array (listArray (0, length arr) arr))
    ) <?> "array literal"

-- = "(", term, ",", term, { ",", term }, ")";
tupleLit :: Parser Value
tupleLit = (do
    -- pos <- getPosition
    tup <- parens (commaSep1 term)
    -- end <- sourceColumn <$> getPosition
    -- let pos' = (mkPos pos) { srcEnd = end }
    return (Tuple (listArray (0, length tup) tup))
    ) <?> "tuple literal"

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
