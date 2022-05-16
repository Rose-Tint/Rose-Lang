module Common.Typing.Kind (
    Kind(..),
    mkKind,
    kindOf,
    kindFromList,
) where

import Data.Word(Word8)

import Common.Typing.Constraint
import Common.Typing.Type
import Pretty


default (Word8, Double)


-- | A `Kind` is basically the type of a type.
-- They work much the same way as haskell's 'kind's.
newtype Kind = Kind Word8
    deriving (Eq)

mkKind :: Integral n => n -> Kind
mkKind = Kind . fromIntegral

kindFromList :: [a] -> Kind
kindFromList = mkKind . length


instance Pretty Kind where
    pretty (Kind n) = " -> " `seps`
        replicate (fromIntegral n + 1) "*"


class HasKind a where
    kindOf :: a -> Kind


instance HasKind Type where
    kindOf (Type _ pars) = Kind
        (fromIntegral (length pars))
    kindOf _ = Kind 0

instance HasKind Constraint where
    kindOf (Constraint _ pars) = Kind
        (fromIntegral (length pars))
