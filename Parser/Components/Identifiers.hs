module Parser.Components.Identifiers (
    validIdLetter,
    smallIdent,
    bigIdent,
    operator,
    infixIdent,
    prefixIdent,
) where

import Text.Parsec (
    many, (<|>), endBy1,
    upper, lower, char,
    (<?>), getPosition, sourceColumn,
    )

import Parser.Components.Internal.LangDef (
    lexeme,
    validIdLetter,
    symbol,
    )
import Parser.Data (
    Parser,
    Var(Var),
    mkPos,
    )


upperIdent, lowerIdent :: Parser String
upperIdent = (:) <$> upper <*> many validIdLetter
lowerIdent = (:) <$> (lower <|> char '_')
    <*> many validIdLetter

-- = ? REGEX "([A-Z][a-zA-Z0-9]*\.)*" ?;
qualifier :: Parser String
qualifier = concat <$> (dotQualIden `endBy1` char '.')
    where
        dotQualIden = (++ ".") <$> upperIdent

-- = qualifier, ? REGEX "[A-Z][a-zA-Z0-9_]*" ?;
bigIdent :: Parser Var
bigIdent = lexeme (do
    pos <- getPosition
    name <- (++) <$> qualifier <*> upperIdent
    end <- sourceColumn <$> getPosition
    return (Var name (mkPos pos end))
    ) <?> "big identifier"

-- = qualifier, ? REGEX "[a-z_][a-zA-Z0-9_]*" ?;
smallIdent :: Parser Var
smallIdent = lexeme (do
    pos <- getPosition
    name <- (++) <$> qualifier <*> lowerIdent
    end <- sourceColumn <$> getPosition
    return (Var name (mkPos pos end))
    ) <?> "small identifier"

oper :: Parser String
oper = (:) <$> symbol <*> many symbol
-- oper = (:) <$> symbol <*> choice [
--         many symbol,
--         (++) <$> many alpha <*> many1 symbol
--     ]

-- = qualifer, symbol - "=", [small-ident], [symbol];
operator :: Parser Var
operator = lexeme (do
    pos <- getPosition
    op <- (++) <$> qualifier <*> oper
    end <- sourceColumn <$> getPosition
    return (Var op (mkPos pos end))
    ) <?> "operator"
    where

-- = operator | "`", small-ident, "`";
infixIdent :: Parser Var
infixIdent = operator <|> (char '`' *> smallIdent <* char '`')
    <?> "infix identifier"

-- = small-ident | "(", operator, ")";
prefixIdent :: Parser Var
prefixIdent = lexeme (do
    pos <- getPosition
    name <- (++) <$> qualifier <*> (lowerIdent <|> oper)
    end <- sourceColumn <$> getPosition
    return (Var name (mkPos pos end))
    ) <?> "prefix identifier"
