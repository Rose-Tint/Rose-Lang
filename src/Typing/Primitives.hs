module Typing.Primitives (
    boolType,
    intType,
    floatType,
    doubleType,
    stringType,
    charType,
    arrayOf,
    tupleOf,
) where

import Typing.Type


boolType :: Type
boolType = Type (prim "Boolean") []

intType :: Type
intType = Type (prim "Int") []

floatType :: Type
floatType = Type (prim "Float") []

doubleType :: Type
doubleType = Type (prim "Double") []

stringType :: Type
stringType = Type (prim "String") []

charType :: Type
charType = Type (prim "Char") []

arrayOf :: Type -> Type
arrayOf = ArrayType

tupleOf :: [Type] -> Type
tupleOf = TupleType
