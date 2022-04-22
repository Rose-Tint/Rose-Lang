module Parser.Components.Terms.Literals (
    intLit,
    octal,
    hex,
    floatLit,
)

-- |@`number` cnt base digitP@ parses a number in base
-- @base@, using @digitP@ as the parser, up to @cnt@
-- places. If @cnt@ is 0, then at least 1 digit is parsed,
-- with no limit
number :: Int -> Int -> Parser Char -> Parser Integer
number 0 !base digitP = foldl' (\x d ->
        base * x + toInteger (digitToInt d)
        ) 0 <$> many1 digitP
number cnt !base digitP = foldl' (\x d ->
        base * x + toInteger (digitToInt d)
        ) 0 <$> count cnt digitP

-- = "-" | "+";
sign :: Num n => Parser (n -> n)
sign = choice [
        char '-' >> return negate,
        char '+' >> return id,
        return id
    ] <?> "sign"

-- = [sign], (binary | octal | hexa | decimal);
intLit :: Parser Value
intLit = (do
    pos <- getPosition
    int <- integer
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (IntLit int pos')
    ) <?> "integer literal"
    where
        -- = [sign], (binary | octal | hexa | decimal);
        integer = sign <*> choice [
                decimal,
                char '0' >> (binary <|> octal <|> hexa)
            ]
        -- = ("b"|"0B"), ("0"|"1"), {("0"|"1")};
        binary = oneOf "bB" >> number 0 2 (oneOf "01")

-- = ("o"|"0O"), oct-digit, {oct-digit};
octal :: Parser Integer
octal = oneOf "oO" >> number 0 8 octDigit

-- = ("x"|"0X"), hex-digit, {hex-digit};
hex :: Parser Integer
hex = do
    oneOf "xX"
    number 0 16 hexDigit

-- = digit, {digit};
decimal :: Parser Integer
decimal = number 0 10 digit

-- = [sign], decimal, exponent, ["f"]
-- | [sign], decimal, ".", decimal, ["f"]
-- | [sign], hexa, hex-exponent, ["f"]
-- | [sign], hexa, ".", hexa, ["f"]
floatLit :: Parser Value
floatLit = lexeme (do
    pos <- getPosition
    flt <- float
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (FloatLit flt pos')
    ) <?> "float literal"
    where
        -- = ("e"|"E"), [sign], decimal;
        expo = (do
            -- optional (char '.')
            oneOf "eE"
            sign <*> decimal <?> "exponent"
            ) <?> "exponent"
        -- = ("p"|"P"), [sign], decimal;
        hexExpr = (do
            -- optional (char '.')
            oneOf "pP"
            sign <*> decimal <?> "exponent"
            ) <?> "exponent"
        -- TODO:
        inner = return 0;
        float = sign <*> inner <* optional (char 'f')

        -- decimal = number 0 10 digit
        -- float = decimal >>= fractExp
        -- fractExp n = do
        --         fract <- option [] fraction
        --         ex <- option [] expon
        --         readDouble (show n ++ fract ++ ex)
        -- expon = (do
        --     oneOf "eE"
        --     sign <- option [] ((:[]) <$> oneOf "-+")
        --     ex <- decimal <?> "exponent"
        --     return (e:sign ++ show ex)
        --     ) <?> "exponent"
        -- fraction = (:) <$> char '.' <*> many1 digit
