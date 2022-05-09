module Common.Typing (
    module T,
    TypeDecl(..),
) where

import Common.Typing.Constraint as T
import Common.Typing.Kind as T
import Common.Typing.Type as T
import Pretty


data TypeDecl = TypeDecl Context Type
    deriving (Eq)


instance Pretty TypeDecl where
    terse (TypeDecl [] typ) = "<"+|typ|+">"
    terse (TypeDecl ctx typ) = "<"+|","`seps`ctx|+":"+|typ|+">"
    pretty (TypeDecl [] typ) = "<"+|typ|+">"
    pretty (TypeDecl ctx typ) = "<"+|", "`seps`ctx|+": "+|typ|+">"
    detailed (TypeDecl [] typ) = "<"+|typ|+">"
    detailed (TypeDecl ctx typ) = "< "+|", "`seps`ctx|+" : "+|typ|+" >"
