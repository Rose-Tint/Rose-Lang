module Analyzer.Prims (
    boolType,
    intLitType, fltLitType, strLitType, chrLitType,
    arrLitType, arrayLitOf
) where

import Parser.Data

intLitType :: Type
{-# INLINE intLitType #-}
intLitType = Type (prim "Int") []

fltLitType :: Type
{-# INLINE fltLitType #-}
fltLitType = Type (prim "Float") []

strLitType :: Type
{-# INLINE strLitType #-}
strLitType = Type (prim "Array") [chrLitType]

chrLitType :: Type
{-# INLINE chrLitType #-}
chrLitType = Type (prim "Char") []

arrLitType :: Type
{-# INLINE arrLitType #-}
arrLitType = Type (prim "Array") [Delayed]

arrayLitOf :: Type -> Type
{-# INLINE arrayLitOf #-}
arrayLitOf t = Type (prim "Array") [t]
