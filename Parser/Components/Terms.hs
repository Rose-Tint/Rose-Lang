module Parser.Components.Terms (
    term,
) where

-- = literal
-- | infix-call
-- | small-ident
-- | lambda
-- | "(", term, ")" 
-- | term, {term};
term :: Parser Value
term = choice [
        intLit, fltLit, chrLit, strLit,
        arrLit, try tupLit,
        try lambda,
        try infixCall,
        try prefixCall,
        ctorCall,
        parens term
    ] <?> "term"]

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

ctorCall :: Parser Value
ctorCall = (do
    name <- bigIdent
    args <- many term
    return (CtorCall name args)
    ) <?> "constructor call"

-- (for now, lambdas will be very limited due to
-- requiring statement)
-- = {small-ident}, "=>", term;
lambda :: Parser Value
lambda = (do
    params <- many smallIdent
    resOper "=>"
    body <- ExprVal <$> term
    ) <?> "lambda"

-- = "[", {term}, "]";
arrLit :: Parser Value
arrLit = (do
    pos <- getPosition
    arr <- brackets (commaSep term)
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (Array (listArray (0, length arr) arr) pos')
    ) <?> "array literal"

-- = "(", term, ",", term, { ",", term }, ")";
tupLit :: Parser Value
tupLit = (do
    pos <- getPosition
    tup <- parens (commaSep1 term)
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (Tuple (listArray (0, length tup) tup) pos')
    ) <?> "tuple literal"
