module Parser.Components.Terms.Literals (
    intLit,
    floatLit,
    charLit,
    stringLit,
)

import Parser.Components.Literals.Terms.Numeric


-- = "//", (hexa | octal | special) | ? ANY CHAR ?;
character :: Parser Char
character = (char '\\' >> (hex' <|> octal' <|> special))
    <|> anyChar <?> "character"
    where
        -- = ? REGEX "\\[xX][0-9a-fA-F]{2}";
        hex' = chr <$> hex
        -- = ? REGEX "\\[xX][0-8]{3}";
        octal' = chr <$> octal
        -- = ? REGEX "[\\abfnrtv'\"]" ?;
        special = do
            ch <- oneOf "\\abfnrtv'\""
            return $ case ch of
                'a' -> '\a'
                'b' -> '\b'
                'f' -> '\f'
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                'v' -> '\v'
                _ -> ch

-- = "'", character, "'";
charLit :: Parser Value
charLit = lexeme (do
    pos <- getPosition
    chr <- between (char '\'') (char '\'') character
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (CharLit chr pos')
    ) <?> "char literal"

-- = """, {character}, """;
stringLit :: Parser Value
stringLit = lexeme (do
    pos <- getPosition
    str <- between (char '"') (char '"')
        (many character)
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (StringLit str pos')
    ) <?> "string literal"
