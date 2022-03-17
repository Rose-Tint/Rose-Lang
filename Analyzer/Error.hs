module Analyzer.Error where

import Parser.Data (Variable)
import SymbolTable.SymbolData
import Typing.Types
import Color
import Pretty



data Error
    = TypeMismatch (Maybe Variable) Type Type
    | TypeMismatch1 (Maybe Variable) Type
    | Undefined Variable Type
    | OtherError String
    deriving (Show, Eq)


data ErrorInfo
    = ErrorInfo {
        eiDefName :: Maybe Symbol,
        eiCalls :: [Symbol]
    }
    deriving (Show)


data Warning
    = ShadowsName Symbol Symbol
    deriving (Show, Eq)
