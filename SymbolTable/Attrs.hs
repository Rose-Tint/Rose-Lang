module SymbolTable.Attrs (
    Attrs,
    emptyAttrs,
    maybeUnused, warnUnused, mustUse, inline,
    cold, deprecated, test,
    mergeAttrs, testAttrs,
    fromPragma, addPragma
) where

import Data.Bits
import Data.Word (Word8)

import Parser.Data (Pragma(..))


default (Word8)


newtype Attrs = As Word8
    deriving (Show, Eq, Ord)


emptyAttrs :: Attrs
{-# INLINE emptyAttrs #-}
emptyAttrs = As 0

maybeUnused :: Attrs
{-# INLINE maybeUnused #-}
maybeUnused = As 1

warnUnused :: Attrs
{-# INLINE warnUnused #-}
warnUnused = As 2

mustUse :: Attrs
{-# INLINE mustUse #-}
mustUse = As 4

inline :: Attrs
{-# INLINE inline #-}
inline = As 8

cold :: Attrs
{-# INLINE cold #-}
cold = As 16

deprecated :: Attrs
{-# INLINE deprecated #-}
deprecated = As 32

test :: Attrs
{-# INLINE test #-}
test = As 64

mergeAttrs :: Attrs -> Attrs -> Attrs
{-# INLINE mergeAttrs #-}
mergeAttrs (As a) (As b) = As (a .|. b)

testAttrs :: Attrs -> Attrs -> Bool
{-# INLINE testAttrs #-}
testAttrs (As a) (As b) = (a .&. b) /= 0

fromPragma :: Pragma -> Attrs
{-# INLINE fromPragma #-}
fromPragma (MaybeUnused _) = maybeUnused
fromPragma (WarnUnused _) = warnUnused
fromPragma (MustUse _) = mustUse
fromPragma (Inline _) = inline
fromPragma (Cold _) = cold
fromPragma (Deprecated _ _) = deprecated
fromPragma (Test _) = test

addPragma :: Pragma -> Attrs -> Attrs
{-# INLINE addPragma #-}
addPragma = mergeAttrs . fromPragma
