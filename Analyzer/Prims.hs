module Analyzer.Prims (
    boolType,
    intLitType, fltLitType,
    strLitType, charType,
    arrayType, arrayOf,
    tupleOf
) where

import Parser.Data

intLitType :: Type
intLitType = Type (prim "Int") []

fltLitType :: Type
fltLitType = Type (prim "Float") []

strLitType :: Type
strLitType = Type (prim "Array") [charType]

charType :: Type
charType = Type (prim "Char") []

arrayType :: Type
arrayType = Type (prim "[]") [Delayed]

arrayOf :: Type -> Type
arrayOf t = Type (prim "[]") [t]

tupleOf :: [Type] -> Type
tupleOf = Type (prim "(,)")
