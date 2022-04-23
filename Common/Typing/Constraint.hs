module Common.Typing.Constraint (
    Constraint(..),
    Context,
) where

import Common.Var


data Constraint = Constraint
    {-# UNPACK #-} !Var -- name
    [Var] -- type-params
    deriving (Eq)

type Context = [Constraint]
