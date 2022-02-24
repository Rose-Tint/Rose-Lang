{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser.LangDef where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Functor.Identity (Identity(..))

import Parser.Data (Value(..))


type Parser a = ParsecT String () Identity a



thornDef :: T.LanguageDef ()
thornDef = haskellStyle {
        T.reservedNames = [
                "pure", "impure", "unsafe",
                "intern", "export",
                "module", "import",
                "fn", "return",
                "if", "else", "case",
                "loop",
                "impl", "trait", "where",
                "match",
                "data"
            ],
        T.reservedOpNames = [
                "=>",
                ":=",
                "|="
            ]
    }


thornTok :: T.TokenParser ()
thornTok = T.makeTokenParser thornDef


iden = T.identifier thornTok
    <?> "identifier"
bigIden = lookAhead upper >> iden
    <?> "big identifier"
smallIden = lookAhead lower >> iden
    <?> "small identifier"
keyword = T.reserved thornTok
operator = T.operator thornTok
resOper = T.reservedOp thornTok
charLit = T.charLiteral thornTok
strLit = StrLit <$> T.stringLiteral thornTok
    <?> "string literal"
intLit = IntLit <$> T.integer thornTok
    <?> "integer literal"
fltLit = FltLit <$> T.float thornTok
    <?> "floating literal"
symbol = T.symbol thornTok
lexeme = T.lexeme thornTok
wspace = T.whiteSpace thornTok
parens = T.parens thornTok
braces = T.braces thornTok
angles = T.angles thornTok
brackets = T.brackets thornTok
semi = T.semi thornTok
comma = T.comma thornTok
dot = T.dot thornTok
semiSep = T.semiSep thornTok
semiSepEnd p = many $ p <* semi
semiSep1 = T.semiSep1 thornTok
commaSep = T.commaSep thornTok
commaSep1 = T.commaSep1 thornTok
