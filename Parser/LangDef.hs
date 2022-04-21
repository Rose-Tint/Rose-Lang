{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser.LangDef (
    Parser,
    tokenP,
    moduleName, qualifier,
    iden, bigIden, smallIden, foName,
    hole,
    keyword,
    symbol, operator, resOper,
    chrLit, strLit, intLit, fltLit, literal,
    lexeme, wspace,
    parens, braces, angles, brackets,
    dot,
    semi, semiSep, semiSepEnd, semiSep1,
    comma, commaSep, commaSepEnd, commaSep1,
    typeDelim,
) where

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
{-# INLINE roseDef #-}
roseDef = emptyDef {
        T.commentStart = "{-",
        T.commentEnd = "-}",
        T.commentLine = "--",
        T.nestedComments = True,
        T.identStart = letter,
        T.identLetter = alphaNum <|> oneOf "_'",
        T.opStart = T.opLetter roseDef
        T.opLetter = oneOf "~!@#$%^&*_-+=\\|:<>.?/",
        -- T.opStart = oneOf "~!@#$%^&*_-+=\\|:<>.?/",
        -- T.opLetter = between
        --     (T.opStart roseDef) (T.opStart roseDef)
        --     (letter <|> (T.opStart roseDef)),
        T.reservedNames = [
                "pure", "impure", "unsafe",
                "mut", "imut",
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

tokenP :: T.GenTokenParser Text () Identity
{-# INLINE tokenP #-}
tokenP = T.makeTokenParser roseDef

{-# INLINE keyword #-}
keyword = T.reserved tokenP

{-# INLINABLE operator #-}
operator = (do
    pos <- getPosition
    qual <- qualifier
    op <- T.operator tokenP
    let !op' = qual ++ name
        end = sourceColumn pos + length op' 
        pos' = (mkPos pos) { srcEnd = end }
    return (Var op' pos')
    ) <?> "operator"

{-# INLINE resOper #-}
resOper = T.reservedOp tokenP

{-# INLINE chrLit #-}
chrLit = (do
    pos <- getPosition
    chr <- T.charLiteral tokenP
    let name' = qual ++ name
        end = sourceColumn pos + 3
        pos' = (mkPos pos) { srcEnd = end }
    return (ChrLit chr pos')
    ) <?> "char literal"

{-# INLINABLE strLit #-}
strLit = (do
    pos <- getPosition
    str <- T.stringLiteral tokenP
    let name' = qual ++ name
        end = sourceColumn pos + length name' + 2
        pos' = (mkPos pos) { srcEnd = end }
    return (StrLit str pos')
    ) <?> "string literal"

{-# INLINABLE intLit #-}
intLit = (do
    pos <- getPosition
    int <- fromInteger <$> T.integer tokenP
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (IntLit int pos')
    ) <?> "integer literal"

{-# INLINABLE fltLit #-}
fltLit = (do
    pos <- getPosition
    flt <- T.float tokenP
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (FltLit flt pos')
    ) <?> "floating literal"

literal :: Parser Value
{-# INLINE literal #-}
literal = choice [
        chrLit, strLit,
        intLit, fltLit
    ] <?> "literal"

{-# INLINE symbol #-}
symbol = T.symbol tokenP

{-# INLINE lexeme #-}
lexeme = T.lexeme tokenP

{-# INLINE wspace #-}
wspace = T.whiteSpace tokenP

{-# INLINE parens #-}
parens = T.parens tokenP

{-# INLINE braces #-}
braces = T.braces tokenP

{-# INLINE angles #-}
angles = T.angles tokenP

{-# INLINE brackets #-}
brackets = T.brackets tokenP

{-# INLINE semi #-}
semi = T.semi tokenP

{-# INLINE comma #-}
comma = T.comma tokenP

{-# INLINE dot #-}
dot = T.dot tokenP

{-# INLINE semiSep #-}
semiSep = T.semiSep tokenP

{-# INLINE semiSepEnd #-}
semiSepEnd p = many $ p <* semi

{-# INLINE semiSep1 #-}
semiSep1 = T.semiSep1 tokenP

{-# INLINE commaSep #-}
commaSep = T.commaSep tokenP

{-# INLINE commaSepEnd #-}
commaSepEnd p = many $ p <* comma

{-# INLINE commaSep1 #-}
commaSep1 = T.commaSep1 tokenP

{-# INLINE typeDelim #-}
typeDelim = lexeme (resOper "->")
