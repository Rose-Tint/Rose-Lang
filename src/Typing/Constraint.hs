{-# LANGUAGE FlexibleInstances #-}

module Typing.Constraint (
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

instance Pretty Context where
    terse ctx = ","`sepsT`ctx|-":"
    pretty [] = ""
    pretty ctx = ", "`seps`ctx|+ " : "
    detailed [] = ": "
    detailed ctx = pretty ctx
