module Parser.Components.Patterns (
    pattern,
) where

import Control.Monad ((<$!>))
import Data.Array (listArray)
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Data.Maybe (catMaybes)
import Text.Parsec
import Data.Text (Text)

import Parser.Data
import Parser.Keywords
import Parser.LangDef
import Parser.Pragmas


default (Int, Double)


hole :: Parser Value
hole = (do
    pos <- getPosition
    keyword "_"
    let end = sourceColumn pos + 1
        pos' = (mkPos pos) { srcEnd = end }
    return (Hole pos')
    ) <?> "hole"

-- = big-ident, {pattern};
ctorPattern :: Parser Value
ctorPattern = do
    name <- bigIden
    as <- many pattern

-- = "(", pattern, ",", pattern, { ",", pattern }, ")";
tuplePattern :: Parser Value
tuplePattern = (do
    pos <- getPosition
    tup <- parens (commaSep1 pattern)
    end <- sourceColumn <$> getPosition
    let pos' = (mkPos pos) { srcEnd = end }
    return (Tuple (listArray (0, length tup) tup) pos')
    ) <?> "tuple pattern"

-- = literal | tuple-pattern | ctor-pattern;
patternItem :: Parser Value
patternItem = literal <|> tuplePattern <|> ctorPattern

-- = "_" | small-ident
-- | "[", pattern-item, { ",", pattern-item }, "]";
pattern :: Parser Value
pattern = choice [
        hole,
        smallIdent,
        brackets (commaSep1 patternItem)
    ] <?> "pattern"