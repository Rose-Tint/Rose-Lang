module Common.Typing.Kind (
    Kind(..),
    getKind,
    kindOf,
) where

import Data.Word(Word8)

import Common.Typing.Constraint
import Common.Typing.Type
import Pretty


default (Word8, Double)


-- | A `Kind` is basically the type of a type.
-- They work much the same way as haskell's 'kind's.
newtype Kind = Kind {-# UNPACK #-} Word8
    deriving (Eq)


instance Pretty Kind where
    pretty (Kind n) = " -> " `seps`
        replicate (n + 1) "*"


class HasKind a where
    getKind :: a -> Kind
    {-# INLINABLE getKind #-}


instance HasKind Type where
    getKind (Type _ pars) = Kind
        (fromIntegral (length pars))
    getKind (Param pars) = Kind
        (fromIntegral (length pars))
    getKind _ = Kind 0

instance HasKind Type where
    getKind (Constraint _ pars) = Kind
        (fromIntegral (length pars))

kindOf :: HasKind a => a -> Kind
{-# INLINE #-}
kindOf = getKind
