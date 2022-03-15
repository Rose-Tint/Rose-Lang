module Analyzer.Prims where

import Parser.Data hiding (Type)
import Typing.Types


integral :: Constraint
integral = Constraint
    (Prim "Integral")
    (Prim "*")


floating :: Constraint
floating = Constraint
    (Prim "Floating")
    (Prim "*")


intLitType :: Type
intLitType = Delayed [integral]


fltLitType :: Type
fltLitType = Delayed [floating]


strLitType :: Type
strLitType = Type (Prim "Array") [chrLitType] []


chrLitType :: Type
chrLitType = Type (Prim "Char") [] []


arrLitType :: Type
arrLitType = Type (Prim "Array") [Delayed []] []


arrayLitOf :: Type -> Type
arrayLitOf t = Type (Prim "Array") [t] []
