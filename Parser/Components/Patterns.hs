module Parser.Components.Patterns (
    pattern,
) where

import Common.SrcPos
import Data.Array (listArray)
import Text.Parsec (
    (<?>), getPosition,
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
    )


hole :: Parser Value
hole = (do
    start <- getPosition
    char '_'
    notFollowedBy validIdLetter
    end <- getPosition
    return (Hole (fromParsecPos start end))
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
