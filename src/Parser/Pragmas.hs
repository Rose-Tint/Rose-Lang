module Parser.Components.Pragmas (
    Pragma(..),
    pragma,
    -- pragmaSeq,
) where

import Common.Item
import Common.Var

data Pragma
    = WarnUnused {-# UNPACK #-} !Var
    | AllowUnused {-# UNPACK #-} !Var
    | Inline {-# UNPACK #-} !Var
    | Deprecated !Item String
    | Test !Item
    deriving (Eq)
