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
    return $! Var fullIdent line start)
    <?> "module name"
    where
        ident = lookAhead upper >> T.identifier thornTok
iden = (do
    pos <- getPosition
    let (line, start) = (sourceLine pos, sourceColumn pos)
    name <- T.identifier thornTok
    return $! Var name line start)
    <?> "identifier"
{-# INLINABLE bigIden #-}
bigIden = lookAhead upper >> iden
    <?> "big identifier"
{-# INLINABLE smallIden #-}
smallIden = lookAhead lower >> iden
    <?> "small identifier"
{-# INLINABLE keyword #-}
keyword = T.reserved thornTok
operator = (do
    pos <- getPosition
    let (line, start) = (sourceLine pos, sourceColumn pos)
    op <- T.operator thornTok
    return $! Var op line start)
    <?> "operator"
{-# INLINABLE resOper #-}
resOper = T.reservedOp thornTok
{-# INLINABLE chrLit #-}
chrLit = ChrLit <$> T.charLiteral thornTok
    <?> "char literal"
{-# INLINABLE strLit #-}
strLit = StrLit <$> T.stringLiteral thornTok
    <?> "string literal"
{-# INLINABLE intLit #-}
intLit = IntLit <$> T.integer thornTok
    <?> "integer literal"
{-# INLINABLE fltLit #-}
fltLit = FltLit <$> T.float thornTok
    <?> "floating literal"
{-# INLINABLE symbol #-}
symbol = T.symbol thornTok
{-# INLINABLE lexeme #-}
lexeme = T.lexeme thornTok
{-# INLINABLE wspace #-}
wspace = T.whiteSpace thornTok
{-# INLINABLE parens #-}
parens = T.parens thornTok
{-# INLINABLE braces #-}
braces = T.braces thornTok
{-# INLINABLE angles #-}
angles = T.angles thornTok
{-# INLINABLE brackets #-}
brackets = T.brackets thornTok
{-# INLINABLE semi #-}
semi = T.semi thornTok
{-# INLINABLE comma #-}
comma = T.comma thornTok
{-# INLINABLE dot #-}
dot = T.dot thornTok
{-# INLINABLE semiSep #-}
semiSep = T.semiSep thornTok
{-# INLINABLE semiSepEnd #-}
semiSepEnd p = many $ p <* semi
{-# INLINABLE semiSep1 #-}
semiSep1 = T.semiSep1 thornTok
{-# INLINABLE commaSep #-}
commaSep = T.commaSep thornTok
{-# INLINABLE commaSep1 #-}
commaSep1 = T.commaSep1 thornTok
