{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser.LangDef where

import Control.Monad ((<$!>))
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Functor.Identity (Identity)

import Parser.Data


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
            (Module Export (Prim $ sourceName pos))
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
            (Module Export (Prim $ sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name)
    return $! Var name pos')
    <?> "identifier"


{-# INLINE bigIden #-}
bigIden = lookAhead upper >> iden
    <?> "big identifier"


{-# INLINE smallIden #-}
smallIden = lookAhead lower >> iden
    <?> "small identifier"


{-# INLINE keyword #-}
keyword = T.reserved thornTok


operator = (do
    pos <- getPosition
    op <- T.operator thornTok
    let pos' = SourcePos
            (Module Export (Prim $ sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length op)
    return $ Var op pos')
    <?> "operator"


{-# INLINE resOper #-}
resOper = T.reservedOp thornTok


{-# INLINE chrLit #-}
chrLit = (do
    pos <- getPosition
    chr <- T.charLiteral thornTok
    let pos' = SourcePos
            (Module Export (Prim $ sourceName pos))
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
            (Module Export (Prim $ sourceName pos))
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
            (Module Export (Prim $ sourceName pos))
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
            (Module Export (Prim $ sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            end
    return $ FltLit flt pos')
    <?> "floating literal"


{-# INLINE symbol #-}
symbol = T.symbol thornTok

{-# INLINE lexeme #-}
lexeme = T.lexeme thornTok

{-# INLINE wspace #-}
wspace = T.whiteSpace thornTok

{-# INLINE parens #-}
parens = T.parens thornTok

{-# INLINE braces #-}
braces = T.braces thornTok

{-# INLINE angles #-}
angles = T.angles thornTok

{-# INLINE brackets #-}
brackets = T.brackets thornTok

{-# INLINE semi #-}
semi = T.semi thornTok

{-# INLINE comma #-}
comma = T.comma thornTok

{-# INLINE dot #-}
dot = T.dot thornTok

{-# INLINE semiSep #-}
semiSep = T.semiSep thornTok

{-# INLINE semiSepEnd #-}
semiSepEnd p = many $ p <* semi

{-# INLINE semiSep1 #-}
semiSep1 = T.semiSep1 thornTok

{-# INLINE commaSep #-}
commaSep = T.commaSep thornTok

{-# INLINE commaSep1 #-}
commaSep1 = T.commaSep1 thornTok
