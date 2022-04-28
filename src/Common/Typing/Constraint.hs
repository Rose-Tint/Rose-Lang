module Common.Typing.Constraint (
    Constraint(..),
    Context,
) where

import Common.Var
import Pretty


data Constraint = Constraint
    {-# UNPACK #-} !Var -- name
    [Var] -- type-params
    deriving (Eq)

type Context = [Constraint]


instance Pretty Constraint where
    pretty (Constraint name types) =
        name|+" "+|" "`seps`types
