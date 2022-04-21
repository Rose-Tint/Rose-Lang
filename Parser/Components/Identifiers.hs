module Parser.Components.Identifiers (
    bigIdent, smallIdent,
    operator,
    infixIdent, prefixIdent,
    identifier
) where

{- MISSING FROM THIS MODULE:
symbol = ? REGEX "(~!@#\$%^&\*-\+=\\\|:<>\.\?/)+" ?;
-}

-- = ? REGEX "([A-Z][a-zA-Z0-9]*\.)*" ?;
qualifier :: Parser String
qualifier = concat <$> many $ try $ do
    lookAhead upper
    qual <- T.identifier tokenP
    resOper "."
    return $! qual ++ "."

-- = qualifier, ? REGEX "[A-Z][a-zA-Z0-9_]*" ?;
bigIdent :: Parser Variable
bigIdent = (do
    pos <- getPosition
    qual <- qualifier
    lookAhead upper
    name <- T.identifier tokenP
    let !name' = qual ++ name
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name')
    return (Var name' pos')
    ) <?> "big identifier"

-- = qualifier, ? REGEX "[a-z_][a-zA-Z0-9_]*" ?;
smallIdent :: Parser Variable
smallIdent = (do
    pos <- getPosition
    qual <- qualifier
    lookAhead lower
    name <- T.identifier tokenP
    let !name' = qual ++ name
        pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name')
    return (Var name' pos')
    ) <?> "small identifier"

-- = qualifer, symbol - "=", [small-ident], [symbol];
operator :: Parser Variable
operator = (do
    pos <- getPosition
    qual <- qualifier
    op <- T.operator tokenP
    let !op' = qual ++ op
        pos' = SourcePos
            (Module Export (Prim $ sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length op')
    return (Var op' pos'))
    <?> "operator"

-- = operator | "`", small-ident, "`";
infixIdent :: Parser Variable
infixIdent = operator <|> (resOper "`" *> smallIdent <* resOper "`")
    <?> "infix identifier"

-- = small-ident | "(", operator, ")";
prefixIdent :: Parser Variable
prefixIdent = smallIdent <|> parens infixIdent
    <?> "prefix identifier"

-- = small-ident | big-ident | "(", operator, ")";
identifier :: Parser Variable
identifier = smallIdent <|> bigIdent <|> parens operator
    <?> "identifier"
