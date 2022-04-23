module Analyzer.State (
    State(..),
    newModuleState,
    newState,
) where

import Analyzer.Error
import Common.SrcPos
import Common.Typing
import Common.Var
import Parser.Components.Imports (Import)
import SymbolTable


data State
    = State {
        stModule :: {-# UNPACK #-} !Var,
        stExpType :: [Type],
        stErrors :: [ErrorMessage],
        stTable :: SymbolTable,
        stImports :: [Import],
        stDefName :: Maybe Symbol,
        stPos :: SrcPos
    }


newModuleState :: String -> State
{-# INLINE newModuleState #-}
newModuleState name = State (prim name) [] []
    emptyTable [] Nothing (newModulePos name)

newState :: State
{-# INLINE newState #-}
newState = newModuleState ""
