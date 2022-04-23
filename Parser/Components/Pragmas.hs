module Parser.Components.Pragmas (
    Pragma(..),
    pragma,
) where

import Text.Parsec (choice, char, (<?>))

import Parser.Components.Terms.Literals
import Parser.Components.Identifiers (prefixIdent)
import Parser.Components.Internal.Item
import Parser.Components.Internal.LangDef (
    brackets, parens,
    lexeme,
    keyword,
    comma,
    )
import Parser.Data (
    Parser,
    Value(StringLit),
    Var,
    )

data Pragma
    = WarnUnused {-# UNPACK #-} !Var
    -- | AllowUnused {-# UNPACK #-} !Var
    | MustUse {-# UNPACK #-} !Var
    | Inline {-# UNPACK #-} !Var
    -- | Cold {-# UNPACK #-} !Var
    | Deprecated !Item String
    | Test !Item
    deriving (Eq)


pragmaWrap :: Parser Pragma -> Parser Pragma
pragmaWrap p = char '#' >> brackets p

-- | Pragmas that can be used on functions
pragma :: Parser Pragma
pragma = (lexeme $ pragmaWrap $ choice [
        warnUnused,
        -- allowUnused,
        mustUse,
        inline,
        -- cold,
        deprecated,
        test
    ]) <?> "pragma"

-- allowUnused :: Parser Pragma
-- allowUnused = do
--     keyword "allow_unused"
--     var <- parens _____
--     return $ MaybeUnused var

-- |Emit a warning if the result of the marked
-- function is ignored
warnUnused :: Parser Pragma
warnUnused = do
    keyword "warn_unused"
    WarnUnused <$> parens prefixIdent

-- |Emit an error when the result of the marked function
-- is not used. More 'extreme' version of `warn_unused`
mustUse :: Parser Pragma
mustUse = do
    keyword "must_use"
    MustUse <$> parens prefixIdent

-- |Strongly encourage the compiler to inline the marked
-- function
inline :: Parser Pragma
inline = do
    keyword "inline"
    Inline <$> parens prefixIdent

-- -- |When used on a function, hint to the
-- -- compiler that this function will not be
-- -- called very often.
-- cold :: Parser Pragma
-- cold = do
--     keyword "cold"
--     Cold <$> parens prefixIdent

-- |Indicates that a function, trait, or datatype is
-- deprecated, with an optional message.
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
test :: Parser Pragma
test = do
    keyword "test"
    Test <$> parens item
