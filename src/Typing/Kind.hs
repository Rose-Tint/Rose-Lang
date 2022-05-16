module Typing.Kind (
    Kind(..),
    kindOf,
) where

import Typing.Constraint
import Typing.Type


data Kind = Kind Int

class HasKind a where
    kindOf :: a -> Kind


instance HasKind Type where
    kindOf (Type _ pars) = Kind
        (fromIntegral (length pars))
    kindOf _ = Kind 0

instance HasKind Constraint where
    kindOf (Constraint _ pars) = Kind
        (fromIntegral (length pars))
