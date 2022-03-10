module Analyzer.Analysis where

import Data.List.NonEmpty (NonEmpty((:|)))

import Analyzer.Analyzer
import Analyzer.Error
import Analyzer.SymbolTable
import Analyzer.Typing
import Parser.Data
import SymbolTable



inferValueType :: Value -> Analyzer SymType
inferValueType (IntLit _) = Type [] Delayed

