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
{-# INLINE roseDef #-}
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

tokenP :: T.GenTokenParser Text () Identity
{-# INLINE tokenP #-}
tokenP = T.makeTokenParser roseDef

{-# INLINE moduleName #-}
moduleName = bigIden <?> "module name"

{-# INLINABLE qualifier #-}
qualifier = (concat <$> many (try $ do
    lookAhead upper
    name <- T.identifier tokenP 
    dot' <- char '.'
    return $! name ++ [dot'])
    ) <?> "source-module qualifier"

{-# INLINABLE iden #-}
iden = (do
    pos <- getPosition
    qual <- qualifier
    name <- T.identifier tokenP
    let name' = qual ++ name
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name)
    return $ name' `seq` Var name' pos'
    ) <?> "identifier"

{-# INLINE bigIden #-}
bigIden = (do
    pos <- getPosition
    qual <- qualifier
    lookAhead upper
    name <- T.identifier tokenP
    let name' = qual ++ name
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name)
    return $ name' `seq` Var name' pos'
    ) <?> "big identifier"

{-# INLINE smallIden #-}
smallIden = (do
    pos <- getPosition
    qual <- qualifier
    lookAhead lower
    name <- T.identifier tokenP
    let name' = qual ++ name
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length name)
    return $ name' `seq` Var name' pos'
    ) <?> "small identifier"

{-# INLINE hole #-}
hole = do
    pos <- getPosition
    keyword "_"
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + 1)
    return $ Hole pos'

{-# INLINE keyword #-}
keyword = T.reserved tokenP

{-# INLINABLE operator #-}
operator = (do
    pos <- getPosition
    op <- T.operator tokenP
    let pos' = SourcePos
            (Module Export (Prim $ sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos + length op)
    return $ Var op pos')
    <?> "operator"

{-# INLINE resOper #-}
resOper = T.reservedOp tokenP

{-# INLINE chrLit #-}
chrLit = (do
    pos <- getPosition
    chr <- T.charLiteral tokenP
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
    str <- T.stringLiteral tokenP
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
    int <- fromInteger <$> T.integer tokenP
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
    flt <- T.float tokenP
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (Module Export (Prim $ sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            end
    return $ FltLit flt pos')
    <?> "floating literal"

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

{-# INLINE commaSep1 #-}
commaSep1 = T.commaSep1 tokenP
