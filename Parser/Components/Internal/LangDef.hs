{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser.Components.Internal.LangDef (
    validIdLetter, symbol,
    keyword, resOper,
    lexeme,
    parens, braces, angles, brackets,
    semi, comma,
    commaSep, commaSepEnd, commaSep1,
) where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

import Parser.Data (Parser)


default (Int, Double)


-- = ? REGEX "(~!@#\$%^&\*-\+=\\\|:<>\.\?/)+" ?;
symbol :: Parser Char
symbol = oneOf "~!@#$%^&*-+=\\|:<>.?/"

validIdLetter :: Parser Char
validIdLetter = alphaNum <|> char '_'

tokenP :: T.GenTokenParser Text () Identity
tokenP = T.makeTokenParser $! emptyDef {
        T.commentStart = "{-",
        T.commentEnd = "-}",
        T.commentLine = "--",
        T.nestedComments = True,
        T.identStart = letter <|> char '_',
        T.identLetter = validIdLetter,
        T.opStart = symbol,
        T.opLetter = symbol,
        T.reservedNames = [
                "pure", "impure", "unsafe",
                "let", "mut", "imut",
                "intern", "extern",
                "module", "import",
                "return",
                "if", "unless", "else",
                "match",
                "loop",
                "impl", "trait",
                "data"
            ],
        T.reservedOpNames = [ "=", ",", "=>" ],
        T.caseSensitive = True
    }

keyword :: String -> Parser ()
keyword = T.reserved tokenP

resOper :: String -> Parser ()
resOper = T.reservedOp tokenP

lexeme :: Parser a -> Parser a
lexeme = T.lexeme tokenP

parens :: Parser a -> Parser a
parens = T.parens tokenP

braces :: Parser a -> Parser a
braces = T.braces tokenP

angles :: Parser a -> Parser a
angles = T.angles tokenP

brackets :: Parser a -> Parser a
brackets = T.brackets tokenP

semi :: Parser ()
semi = T.semi tokenP >> return ()

comma :: Parser ()
comma = T.comma tokenP >> return ()

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep tokenP

commaSepEnd :: Parser a -> Parser [a]
commaSepEnd p = many $ p <* comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 tokenP
