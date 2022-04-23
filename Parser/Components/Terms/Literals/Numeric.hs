module Parser.Components.Terms.Literals.Numeric (
    intLit,
    octal,
    hex,
    floatLit,
) where

import Data.Char (digitToInt)
import Data.Int (Int64)
import Data.List (foldl')
import Text.Parsec (
    many1, count, choice, (<|>),
    optional,
    (<?>), getPosition,
    octDigit, hexDigit, digit,
    char, oneOf,
    )

import Common.SrcPos
import Parser.Components.Internal.LangDef (lexeme)
import Parser.Data (
    Parser,
    Value(IntLit, FloatLit),
    )

-- |@`number` cnt base digitP@ parses a number in base
-- @base@, using @digitP@ as the parser, up to @cnt@
-- places. If @cnt@ is 0, then at least 1 digit is parsed,
-- with no limit
number :: Int -> Int64 -> Parser Char -> Parser Int64
number 0 base digitP = foldl' (\ !x d ->
    base * x + fromIntegral (digitToInt d)
    ) (0 :: Int64) <$> many1 digitP
number cnt base digitP = foldl' (\ !x d ->
    base * x + fromIntegral (digitToInt d)
    ) (0 :: Int64) <$> count cnt digitP

-- = "-" | "+";
sign :: Num n => Parser (n -> n)
sign = choice [
        char '-' >> return negate,
        char '+' >> return id,
        return id
    ] <?> "sign"

-- = [sign], (binary | octal | hexa | decimal);
intLit :: Parser Value
intLit = lexeme (do
    start <- getPosition
    int <- integer
    end <- getPosition
    return (IntLit int (fromParsecPos start end))
    ) <?> "integer literal"
    where
        -- = [sign], (binary | octal | hexa | decimal);
        integer = sign <*> choice [
                decimal,
                char '0' >> (binary <|> octal <|> hex)
            ]
        -- = ("b"|"0B"), ("0"|"1"), {("0"|"1")};
        binary = oneOf "bB" >> number 0 2 (oneOf "01")

-- = ("o"|"0O"), oct-digit, {oct-digit};
octal :: Parser Int64
octal = oneOf "oO" >> number 0 8 octDigit

-- = ("x"|"0X"), hex-digit, {hex-digit};
hex :: Parser Int64
hex = do
    oneOf "xX"
    number 0 16 hexDigit

-- = digit, {digit};
decimal :: Parser Int64
decimal = number 0 10 digit

-- = [sign], decimal, exponent, ["f"]
-- | [sign], decimal, ".", decimal, ["f"]
-- | [sign], hexa, hex-exponent, ["f"]
-- | [sign], hexa, ".", hexa, ["f"]
floatLit :: Parser Value
floatLit = lexeme (do
    start <- getPosition
    flt <- float
    end <- getPosition
    return (FloatLit flt (fromParsecPos start end))
    ) <?> "float literal"
    where
        -- -- = ("e"|"E"), [sign], decimal;
        -- expo = (do
        --     -- optional (char '.')
        --     oneOf "eE"
        --     sign <*> decimal <?> "exponent"
        --     ) <?> "exponent"
        -- -- = ("p"|"P"), [sign], decimal;
        -- hexExpr = (do
        --     -- optional (char '.')
        --     oneOf "pP"
        --     sign <*> decimal <?> "exponent"
        --     ) <?> "exponent"
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
