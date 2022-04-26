module Parser.Components.Pragmas (
    Pragma(..),
    pragma,
    -- pragmaSeq,
) where

import Text.Parsec (choice, char, (<?>), (<|>))

import Common.Item
import Common.Var
import Parser.Components.Terms.Literals
import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef (
    brackets, parens,
    lexeme,
    keyword,
    comma,-- commaSep1,
    )
import Parser.Data (
    Parser,
    Value(StringLit),
    )

data Pragma
    = WarnUnused {-# UNPACK #-} !Var
    | AllowUnused {-# UNPACK #-} !Var
    | Inline {-# UNPACK #-} !Var
    | Deprecated !Item String
    | Test !Item
    deriving (Eq)


pragmaWrap :: Parser Pragma -> Parser Pragma
pragmaWrap p = char '#' >> brackets p

-- | Pragmas that can be used on functions
-- = "#","[", directive, "]";
pragma :: Parser Pragma
pragma = lexeme (pragmaWrap directive) <?> "pragma"

-- -- = "#","[", directive, { ",", directive }, "]";
-- pragmaSeq :: Parser Pragma
-- pragmaSeq = lexeme (pragmaWrap (PragmaSeq <$> 
--     (commaSep1 directive)))
--     <?> "pragma sequence"

-- = allow-unused
-- | warn-unused
-- | inline
-- | deprecated
-- | test;
directive :: Parser Pragma
directive = choice [
        allowUnused,
        warnUnused,
        inline,
        deprecated,
        test
    ] <?> "pragma directive"

-- = "allow_unused","(", identifier, ")";
allowUnused :: Parser Pragma
allowUnused = do
    keyword "allow_unused"
    var <- parens (prefixIdent <|> bigIdent)
    return (AllowUnused var)

-- |Emit a warning if the result of the marked
-- function is ignored
-- = "warn_unused","(", prefix-ident, ")";
warnUnused :: Parser Pragma
warnUnused = do
    keyword "warn_unused"
    WarnUnused <$> parens prefixIdent

-- |Strongly encourage the compiler to inline the marked
-- function
-- = "inline","(", prefix-ident, ")";
inline :: Parser Pragma
inline = do
    keyword "inline"
    Inline <$> parens prefixIdent

-- |Indicates that a function, trait, or datatype is
-- deprecated, with an optional message.
-- = "deprecated","(", identifier, [ ",", string ] ")";
deprecated :: Parser Pragma
deprecated = do
    keyword "deprecated"
    (var, (StringLit msg _)) <- parens $ do
        var <- item
        comma
        msg <- stringLit
        return (var, msg)
    return (Deprecated var msg)

-- |Ignore unless compiling in testing mode.
-- = "test","(", identifier, ")";
test :: Parser Pragma
test = do
    keyword "test"
    Test <$> parens item

-- -- = "#","[", ("cold"|"warm"), "]";
-- heatPragma :: Parser Bool
-- heatPragma = pragmaWrap $ choice [
--         keyword "cold" >> True,
--         keyword "heat" >> False,
--     ] <?> "heat-pragma"
