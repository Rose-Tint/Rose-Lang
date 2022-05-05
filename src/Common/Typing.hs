module Common.Typing (
    module Common.Typing,
) where

import Common.Typing.Constraint as Common.Typing
import Common.Typing.Type as Common.Typing
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
