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
{-# INLINE boolType #-}
boolType = TypeCon (prim "Bool") []

intType :: Type
{-# INLINE intType #-}
intType = TypeCon (prim "Int") []

floatType :: Type
{-# INLINE floatType #-}
floatType = TypeCon (prim "Float") []

doubleType :: Type
{-# INLINE doubleType #-}
doubleType = TypeCon (prim "Double") []

stringType :: Type
{-# INLINE stringType #-}
stringType = TypeCon (prim "String") []

charType :: Type
{-# INLINE charType #-}
charType = TypeCon (prim "Char") []

arrayOf :: Type -> Type
{-# INLINE arrayOf #-}
arrayOf = ArrayType

tupleOf :: [Type] -> Type
{-# INLINE tupleOf #-}
tupleOf = TupleType
