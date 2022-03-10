module Analyzer.Error where

import Parser.Data
import SymbolTable.SymbolData (Symbol)



data Error
    = TypeMismatch Variable Type Type
    | OtherError String


data ErrorInfo
    = ErrorInfo {
        eiDefName :: Maybe Symbol,
        eiCalls :: [Symbol]
    }


data Warning
    = ShadowsName Symbol Symbol
