module Parser.Pragmas (
    pragma,
    -- -- Coverage
    -- allowUnused, warnUnused, mustUse,
    -- -- Code generation
    -- inline, cold,
    -- -- Other
    -- deprecated, test,
) where

import Parser.Data (Parser, Pragma(..))
import Parser.LangDef (
    brackets,
    parens,
    keyword
    )


-- The reason that some the following pragma parsers have
-- leading underscores is because they are not wrapped by
-- `pragmaWrap`, and thus can be faster when used with
-- `choice` or `(<|>)`


pragmaWrap :: Parser Pragma -> Parser Pragma
{-# INLINE pragmaWrap #-}
pragmaWrap p = char '#' >> brackets p

-- | Pragmas that can be used on functions
pragma :: Parser Pragma
{-# INLINABLE pragma #-}
pragma = pragmaWrap $ choice [
        _allowUnused,
        _warnUnused,
        _mustUse,
        _inline,
        _cold,
        _deprecated,
        _test
    ]

-- |When used on a variable or function, omit
-- any warnings about the variable or function being
-- unused
--
-- Not yet implemented:
-- When used on a function call whose function was
-- marked with `#[warn_unused(...)]`, omit that warning
allowUnused, _allowUnused :: Parser Pragma
{-# INLINE allowUnused #-}
allowUnused = pragmaWrap _allowUnused
{-# INLINE _allowUnused #-}
_allowUnused = do
    keyword "allow_unused"
    var <- parens iden
    return $ MaybeUnused var

-- |Emit a warning if the result of the marked
-- function is ignored
warnUnused, _warnUnused :: Parser Pragma
{-# INLINE warnUnused #-}
warnUnused = pragmaWrap _warnUnused
{-# INLINE _warnUnused #-}
_warnUnused = do
    keyword "warn_unused"
    var <- parens smallIden
    return $ WarnUnused var

-- |Emit an error when the result of the marked function
-- is not used. More 'extreme' version of `warn_unused`
mustUse, _mustUse :: Parser Pragma
{-# INLINE mustUse #-}
mustUse = pragmaWrap _mustUse
{-# INLINE _mustUse #-}
_mustUse = do
    keyword "must_use"
    var <- parens smallIden
    return $ MustUse var

-- |Strongly encourage the compiler to inline
-- the marked function
inline, _inline :: Parser Pragma
{-# INLINE inline #-}
inline = pragmaWrap _inline
{-# INLINE _inline #-}
_inline = do
    keyword "inline"
    var <- parens smallIden
    return $ Inline var

-- |When used on a function, hint to the
-- compiler that this function will not be
-- called very often.
--
-- Not yet implemented:
-- When used on an if/else or match pattern,
-- hint to the compiler that this branch or pattern
-- will less likely than the others.
cold, _cold :: Parser Pragma
{-# INLINE cold #-}
cold = pragmaWrap _cold
{-# INLINE _cold #-}
_cold = do
    keyword "cold"
    var <- parens smallIden
    return $ Cold var

-- |Indicates that a function, trait, or datatype is
-- deprecated, with an optional message.
deprecated, _deprecated :: Parser Pragma
{-# INLINE deprecated #-}
deprecated = pragmaWrap _deprecated
{-# INLINE _deprecated #-}
_deprecated = do
    keyword "deprecated"
    (var, msg) <- parens $ do
        var <- iden
        comma
        msg <- strLit
        return (var, msg)
    return $ Deprecated var msg

-- |Ignore unless compiling in testing mode.
test, _test :: Parser Pragma
{-# INLINE test #-}
test = pragmaWrap _test
{-# INLINE _test #-}
_test = do
    keyword "test"
    var <- parens iden
    return $ Test var
