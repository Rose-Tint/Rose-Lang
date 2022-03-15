module Analyzer.State where

import Analyzer.Error
import CmdLine (CmdLine)
import SymbolTable
import Typing.Types



data State
    = State {
        stCmdLine :: !CmdLine,
        stExpType :: [Type],
        stErrInfo :: !ErrorInfo,
        stErrors :: ![(ErrorInfo, Error)],
        stWarnings :: ![(ErrorInfo, Warning)],
        stTable :: !SymbolTable,
        stImports :: ![Module]
    }
