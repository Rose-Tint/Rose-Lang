module Analyzer.Analysis where

-- import Control.Monad ((<$!>))
-- import Data.List.NonEmpty (NonEmpty((:|)))

import Analyzer.Analyzer
-- import Analyzer.Error
import Analyzer.Prims
-- import Analyzer.SymbolTable
import Parser.Data
import SymbolTable



inferValueType :: Value -> Analyzer SymType
inferValueType (IntLit _) = return intLitType
inferValueType (FltLit _) = return fltLitType
inferValueType (ChrLit _) = return chrLitType
inferValueType (StrLit _) = return strLitType
-- inferValueType (VarVal var) = sdType <$!> searchScopeds var
inferValueType _ = return $ Delayed []
