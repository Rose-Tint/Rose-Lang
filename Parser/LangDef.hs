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

{-# INLINE resOper #-}
resOper = T.reservedOp tokenP

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
