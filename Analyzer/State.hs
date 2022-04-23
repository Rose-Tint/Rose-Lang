module Analyzer.State (
    Position(..),
    State(..),
    newModuleState,
    newState,
) where

import Analyzer.Error
import Parser.Components.Imports (Import)
import Parser.Data
import SymbolTable


data State
    = State {
        stModule :: {-# UNPACK #-} !Var,
        stExpType :: [Type],
        stErrors :: [ErrorMessage],
        stTable :: SymbolTable,
        stImports :: [Import],
        stDefName :: Maybe Symbol,
        stPosition :: Position
    }


newModuleState :: String -> State
{-# INLINE newModuleState #-}
newModuleState name = State (prim name) [] []
    emptyTable [] Nothing (newPosition name)

newState :: State
{-# INLINE newState #-}
newState = newModuleState ""
