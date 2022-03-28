module Analyzer.State (
    Position(..),
    State(..),
    newModuleState,
    newState,
) where

import Analyzer.Error
import CmdLine (CmdLine)
import Parser.Data (Position(..), newPosition)
import SymbolTable
import Typing.Types



data State
    = State {
        stModule :: Module,
        stCmdLine :: !CmdLine,
        stExpType :: [Type],
        stRetType :: Maybe Type,
        stErrors :: [ErrorMessage],
        stTable :: !SymbolTable,
        stImports :: [Module],
        stDefName :: Maybe Symbol,
        stCalls :: [Symbol],
        stPosition :: Position
    }



newModuleState :: CmdLine -> String -> State
{-# INLINE newModuleState #-}
newModuleState cmd modName = State ModUnknown cmd [] Nothing []
    emptyTable [] Nothing [] (newPosition modName)


newState :: CmdLine -> State
{-# INLINE newState #-}
newState cmd = State ModUnknown cmd [] Nothing []
    emptyTable [] Nothing [] (newPosition "")
