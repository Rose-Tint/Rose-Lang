module Parser.Components.Patterns (
    pattern,
) where

import Data.Array (listArray)
import Text.Parsec (
    (<?>), getPosition, sourceColumn,
    char,
    many, choice, (<|>),
    notFollowedBy,
    )

import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef (
    parens,
    brackets,
    commaSep1,
    )
import Parser.Components.Terms
import Parser.Data (
    Parser,
    Value(Hole, Tuple, CtorCall, VarVal),
    mkPos,
    )


hole :: Parser Value
hole = (do
    pos <- getPosition
    char '_'
    notFollowedBy validIdLetter
    return (Hole (mkPos pos (sourceColumn pos + 1)))
    ) <?> "hole"

-- = big-ident, {pattern};
ctorPattern :: Parser Value
ctorPattern = do
    name <- bigIdent
    as <- many pattern
    return (CtorCall name as)

-- = "(", pattern, ",", pattern, { ",", pattern }, ")";
tuplePattern :: Parser Value
tuplePattern = (do
    tup <- parens (commaSep1 pattern)
    return (Tuple (listArray (0, length tup) tup))
    ) <?> "tuple pattern"

-- = literal | tuple-pattern | ctor-pattern;
patternItem :: Parser Value
patternItem = literal <|> tuplePattern <|> ctorPattern

-- = "_" | small-ident
-- | "[", pattern-item, { ",", pattern-item }, "]";
pattern :: Parser Value
pattern = choice [
        hole,
        VarVal <$> smallIdent,
        brackets patternItem
        -- brackets (commaSep1 patternItem),
    ] <?> "pattern"
