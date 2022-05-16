{-# LANGUAGE FlexibleInstances #-}

module Typing.Constraint (
    Constraint(..),
    Context(..),
) where

import Common.Var
import Text.Pretty


data Constraint = Constraint
    {-# UNPACK #-} !Var -- name
    [Var] -- type-params
    deriving (Eq)

newtype Context = Ctx [Constraint]


instance Pretty Constraint where
    pretty (Constraint name types) =
        name|+" "+|" "`seps`types

instance Pretty Context where
    terse (Ctx []) = ""
    terse (Ctx cs) = ","`sepsT`cs|-":"
    pretty (Ctx []) = ""
    pretty (Ctx cs) = ", "`seps`cs|+ " : "
