module Typing.Kind (
    Kind(..),
    mkKind,
    kindOf,
    kindFromList,
) where

import Pretty


default (Word8, Double)


data Kind

class HasKind a where
    kindOf :: a -> Kind


instance HasKind Type where
    kindOf (Type _ pars) = Kind
        (fromIntegral (length pars))
    kindOf _ = Kind 0

instance HasKind Constraint where
    kindOf (Constraint _ pars) = Kind
        (fromIntegral (length pars))
