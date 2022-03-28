{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser.LangDef where

import Control.Monad ((<$!>))
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Functor.Identity (Identity)

import Parser.Data (Value(..), Variable(..), Position(..))


default (Int, Double)


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
    top <- ident
    rest <- many (try (dot >> pure ('.':) <*> ident))
    let fullIdent = top ++ concat rest
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length fullIdent)
    return $! Var fullIdent pos')
    <?> "module name"
    where
        ident = lookAhead upper >> T.identifier thornTok
iden = (do
    pos <- getPosition
    name <- T.identifier thornTok
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name)
    return $! Var name pos')
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
    op <- T.operator thornTok
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length op)
    return $ Var op pos')
    <?> "operator"
{-# INLINABLE resOper #-}
resOper = T.reservedOp thornTok
{-# INLINABLE chrLit #-}
chrLit = (do
    pos <- getPosition
    chr <- T.charLiteral thornTok
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + 2)
    return $ ChrLit chr pos')
    <?> "char literal"
{-# INLINABLE strLit #-}
strLit = (do
    pos <- getPosition
    str <- T.stringLiteral thornTok
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length str + 2)
    return $ StrLit str pos')
    <?> "string literal"
{-# INLINABLE intLit #-}
intLit = (do
    pos <- getPosition
    int <- T.integer thornTok
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            end
    return $ IntLit int pos')
    <?> "integer literal"
{-# INLINABLE fltLit #-}
fltLit = (do
    pos <- getPosition
    flt <- T.float thornTok
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            end
    return $ FltLit flt pos')
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
