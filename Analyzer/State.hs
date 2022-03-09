module Analyzer.State where

import SymbolTable.SymbolTable



data ErrorInfo


data State
    = State {
        stErrInfo :: ErrorInfo,
        stTable :: !SymbolTable
    }
