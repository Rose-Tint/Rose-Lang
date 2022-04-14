module Analyzer.Prims (
    Analyzer.Prims.boolType,
    integral, floating,
    intLitType, fltLitType, strLitType, chrLitType,
    arrLitType, arrayLitOf
) where

import Parser.Data hiding (Type)
import Typing.Types


boolType :: Type
{-# INLINE boolType #-}
boolType = Type (Prim "Boolean") [] []

integral :: Constraint
{-# INLINE integral #-}
integral = Constraint
    (Prim "Integral")
    (Prim "*")

floating :: Constraint
{-# INLINE floating #-}
floating = Constraint
    (Prim "Floating")
    (Prim "*")

intLitType :: Type
{-# INLINE intLitType #-}
intLitType = Delayed [integral]

fltLitType :: Type
{-# INLINE fltLitType #-}
fltLitType = Delayed [floating]

strLitType :: Type
{-# INLINE strLitType #-}
strLitType = Type (Prim "Array") [chrLitType] []

chrLitType :: Type
{-# INLINE chrLitType #-}
chrLitType = Type (Prim "Char") [] []

arrLitType :: Type
{-# INLINE arrLitType #-}
arrLitType = Type (Prim "Array") [Delayed []] []

arrayLitOf :: Type -> Type
{-# INLINE arrayLitOf #-}
arrayLitOf t = Type (Prim "Array") [t] []
