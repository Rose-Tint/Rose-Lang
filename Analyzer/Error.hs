module Analyzer.Error where

import Parser.Data (Variable)
import SymbolTable.SymbolData
import Typing.Types



data Error
    = TypeMismatch (Maybe Variable) Type Type
    | TypeMismatch1 (Maybe Variable) Type
    | Undefined Variable Type
    | OtherError String


data ErrorInfo
    = ErrorInfo {
        eiDefName :: Maybe Symbol,
        eiCalls :: [Symbol]
    }


data Warning
    = ShadowsName Symbol Symbol
