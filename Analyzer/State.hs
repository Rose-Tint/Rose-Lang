module Analyzer.State where

import Analyzer.Error
import CmdLine (CmdLine)
import Parser.Data
import SymbolTable



data State
    = State {
        stCmdLine :: !CmdLine,
        stExpType :: [Type],
        stErrInfo :: !ErrorInfo,
        stErrors :: ![(ErrorInfo, Error)],
        stWarnings :: ![(ErrorInfo, Warning)],
        stTable :: !SymbolTable
    }
