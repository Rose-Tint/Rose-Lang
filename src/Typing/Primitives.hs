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

import Common.Var
import Typing.Type


boolType :: Type
boolType = TypeCon (prim "Bool") []

intType :: Type
intType = TypeCon (prim "Int") []

floatType :: Type
floatType = TypeCon (prim "Float") []

doubleType :: Type
doubleType = TypeCon (prim "Double") []

stringType :: Type
stringType = TypeCon (prim "String") []

charType :: Type
charType = TypeCon (prim "Char") []

arrayOf :: Type -> Type
arrayOf = ArrayType

tupleOf :: [Type] -> Type
tupleOf = TupleType
