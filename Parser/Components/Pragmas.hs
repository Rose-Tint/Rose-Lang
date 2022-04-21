module Parser.Pragmas (
    pragma,
) where


data Pragma
    = MaybeUnused {-# UNPACK #-} !Var
    | WarnUnused {-# UNPACK #-} !Var
    | MustUse {-# UNPACK #-} !Var
    | Inline {-# UNPACK #-} !Var
    | Cold {-# UNPACK #-} !Var
    | Deprecated {-# UNPACK #-} !Var String
    | Test {-# UNPACK #-} !Var
    deriving (Show, Eq, Ord)


pragmaWrap :: Parser Pragma -> Parser Pragma
pragmaWrap p = resOper "#" >> brackets p

-- | Pragmas that can be used on functions
pragma :: Parser Pragma
pragma = pragmaWrap $ choice [
        allowUnused,
        warnUnused,
        mustUse,
        inline,
        cold,
        deprecated,
        test
    ]

-- |When used on a variable or function, omit
-- any warnings about the variable or function being
-- unused
--
-- Not yet implemented:
-- When used on a function call whose function was
-- marked with `#[warn_unused(...)]`, omit that warning
allowUnused :: Parser Pragma
allowUnused = do
    keyword "allow_unused"
    var <- parens identifier
    return $ MaybeUnused var

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

-- |Strongly encourage the compiler to inline
-- the marked function
inline :: Parser Pragma
inline = do
    keyword "inline"
    Inline <$> parens prefixIdent

-- |When used on a function, hint to the
-- compiler that this function will not be
-- called very often.
--
-- Not yet implemented:
-- When used on an if/else or match pattern,
-- hint to the compiler that this branch or pattern
-- will less likely than the others.
cold :: Parser Pragma
cold = do
    keyword "cold"
    Cold <$> parens prefixIdent

-- |Indicates that a function, trait, or datatype is
-- deprecated, with an optional message.
deprecated :: Parser Pragma
deprecated = do
    keyword "deprecated"
    (var, (StrLit msg _)) <- parens $ do
        var <- identifier
        comma
        msg <- strLit
        return (var, msg)
    return (Deprecated var msg)

-- |Ignore unless compiling in testing mode.
test :: Parser Pragma
test = do
    keyword "test"
    Test <$> parens identifier
