module Analyzer.Prims where

import Parser.Data
import SymbolTable.SymbolData


integral :: Constraint
integral = Constraint
    (Prim "Integral")
    (Prim "*")


floating :: Constraint
floating = Constraint
    (Prim "Floating")
    (Prim "*")


intLitType :: SymType
intLitType = Delayed [integral]


fltLitType :: SymType
fltLitType = Delayed [floating]


strLitType :: SymType
strLitType = Type [] (TerminalType (RealType (Prim "Array"))
    [TerminalType (RealType (Prim "Char")) []])


chrLitType :: SymType
chrLitType = Type [] (TerminalType (RealType (Prim "Char")) [])
