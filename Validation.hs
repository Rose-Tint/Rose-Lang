module Validation where

import Parser.Data



data VldError
    = TypeMismatch Variable Type
    | ConsNotMet (Constraint, Type)
    | IdenResol String


data Attribute
    = TypeAttr Type
    | MemLocAttr
    | ValueAttr
    | NameAttr
    | Component

data Environment
    = Environment {
        envTypes  :: [Type],
        envTraits :: [String]
        envVars   :: [Variable]
    }

data Visitor
    = Valid Expr [Attribute]
    | Invalid [VldError]

