module Common.Typing (
    module Common.Typing,
) where

import Common.Typing.Constraint as Common.Typing
import Common.Typing.Type as Common.Typing


data TypeDecl = TypeDecl Context Type
    deriving (Eq)
