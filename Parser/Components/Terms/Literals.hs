module Parser.Components.Terms.Literals (
    intLit,
    floatLit,
    charLit,
    stringLit,
) where

import Data.Char (chr)
import Text.Parsec (
    many, between, (<|>),
    char, anyChar, oneOf,
    (<?>), getPosition,
    )

import Common.SrcPos
import Parser.Components.Terms.Literals.Numeric
import Parser.Data (
    Parser,
    Value(CharLit, StringLit),
    )
import Parser.Components.Internal.LangDef (lexeme)



-- = "//", (hexa | octal | special) | ? ANY CHAR ?;
character :: Parser Char
character = (char '\\' >> (hex' <|> octal' <|> special))
    <|> anyChar <?> "character"
    where
        -- = ? REGEX "\\[xX][0-9a-fA-F]{2}";
        hex' = chr . fromEnum <$> hex
        -- = ? REGEX "\\[xX][0-8]{3}";
        octal' = chr . fromEnum <$> octal
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
    start <- getPosition
    ch <- between (char '\'') (char '\'') character
    end <- getPosition
    return (CharLit ch (fromParsecPos start end))
    ) <?> "char literal"

-- = """, {character}, """;
stringLit :: Parser Value
stringLit = lexeme (do
    pos <- getPosition
    str <- between (char '"') (char '"')
        (many character)
    end <- getPosition
    return (StringLit str (fromParsecPos pos end))
    ) <?> "string literal"
