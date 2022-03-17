module Analyzer.Prims where

import Parser.Data hiding (Type)
import Typing.Types


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