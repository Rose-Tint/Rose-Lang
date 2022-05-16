module Typing.TypeDecl (
    TypeDecl(..),
    typeDecl
) where

import Text.Pretty
import Typing.Constraint
import Typing.Type


data TypeDecl = TypeDecl Context Type


typeDecl :: [Constraint] -> Type -> TypeDecl
typeDecl = TypeDecl . Ctx


instance Pretty TypeDecl where
    pretty (TypeDecl ctx typ) = "<"+|ctx|+|typ|+">"

