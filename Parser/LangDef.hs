{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser.LangDef where

import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Functor.Identity (Identity)

import Parser.Data (Value(..), Variable(..))


type Parser a = ParsecT Text () Identity a



roseDef :: T.GenLanguageDef Text () Identity
roseDef = emptyDef {
        T.commentStart = "{-",
        T.commentEnd = "-}",
        T.commentLine = "--",
        T.nestedComments = True,
        T.identStart = letter,
        T.identLetter = alphaNum <|> oneOf "_'",
        T.opStart = T.opLetter roseDef,
        T.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
        T.reservedNames = [
                "pure", "impure", "unsafe",
                "let", "mut", "imut",
                "intern", "export",
                "module", "import",
                "return",
                "if", "else", "match",
                "loop",
                "impl", "trait",
                "data"
            ],
        T.reservedOpNames = [
                "=>",
                ":=",
                "|="
            ],
        T.caseSensitive = True
    }


thornTok :: T.GenTokenParser Text () Identity
thornTok = T.makeTokenParser roseDef


moduleName = (do
    pos <- getPosition
    let (line, start) = (sourceLine pos, sourceColumn pos)
    top <- ident
    rest <- many (try (dot >> pure ('.':) <*> ident))
    let fullIdent = top ++ concat rest
    let end = start + length fullIdent
    return $! Var fullIdent line start end)
    <?> "module name"
    where
        ident = lookAhead upper >> T.identifier thornTok
iden = (do
    pos <- getPosition
    let (line, start) = (sourceLine pos, sourceColumn pos)
    name <- T.identifier thornTok
    let end = start + length name
    return $! Var name line start end)
    <?> "identifier"
bigIden = lookAhead upper >> iden
    <?> "big identifier"
smallIden = lookAhead lower >> iden
    <?> "small identifier"
keyword = T.reserved thornTok
operator = (do
    pos <- getPosition
    let (line, start) = (sourceLine pos, sourceColumn pos)
    op <- T.operator thornTok
    end <- sourceColumn <$> getPosition
    return $! Var op line start end)
    <?> "operator"
resOper = T.reservedOp thornTok
chrLit = ChrLit <$> T.charLiteral thornTok
    <?> "char literal"
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
