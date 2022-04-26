module Parser.Components.Identifiers (
    moduleName,
    validIdLetter,
    smallIdent,
    bigIdent,
    operator,
    infixIdent,
    prefixIdent,
) where

import Control.Monad (when)
import Text.Parsec

import Common.SrcPos
import Common.Var
import Parser.Components.Internal.LangDef
import Parser.Data (Parser)


upperIdent, lowerIdent :: Parser String
upperIdent = (:) <$> upper <*> many validIdLetter
lowerIdent = (:) <$> lower <*> many validIdLetter

moduleName :: Parser String
moduleName = (varName <$> bigIdent)
    <?> "module name"

-- = ? REGEX "([A-Z][a-zA-Z0-9]*\.)*" ?;
qualifier :: Parser String
qualifier = (concat <$> many (try $ (++) <$>
    upperIdent <*> ((:[]) <$> char '.'
    ))) <?> "qualifier"

-- = qualifier, ? REGEX "[A-Z][a-zA-Z0-9_]*" ?;
bigIdent :: Parser Var
bigIdent = lexeme (do
    start <- getPosition
    qual <- qualifier
    name <- upperIdent
    end <- getPosition
    let !name' = qual ++ name
        pos = fromParsecPos start end
    return (Var name' pos)
    ) <?> "big identifier"

-- = qualifier, ? REGEX "[a-z_][a-zA-Z0-9_]*" ?;
smallIdent :: Parser Var
smallIdent = lexeme (do
    start <- getPosition
    qual <- qualifier
    name <- lowerIdent
    when (isResName name) $
        unexpected ("keyword " ++ show name)
    end <- getPosition
    let !name' = qual ++ name
        pos = fromParsecPos start end
    return (Var name' pos)
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
    start <- getPosition
    qual <- qualifier
    op <- oper
    when (isResOper op) $
        unexpected ("reserved operator " ++ show op)
    end <- getPosition
    let !name = qual ++ op
        pos = fromParsecPos start end
    return (Var name pos)
    ) <?> "operator"
    where

-- = operator | "`", small-ident, "`";
infixIdent :: Parser Var
infixIdent = lexeme (
    operator <|> (char '`' *> smallIdent <* char '`')
    ) <?> "infix identifier"

-- = small-ident | "(", operator, ")";
prefixIdent :: Parser Var
prefixIdent = smallIdent <|> parens operator
