module Analyzer.State (
    Position(..),
    State(..),
    newModuleState,
    newState,
) where

import Analyzer.Error
import Parser.Data (Module(..), Position(..), newPosition)
import SymbolTable
import Typing.Types



data State
    = State {
        stModule :: Module,
        stExpType :: [Type],
        stErrors :: [ErrorMessage],
        stTable :: !SymbolTable,
        stImports :: [Module],
        stDefName :: Maybe Symbol,
        stPosition :: Position
    }



newModuleState :: String -> State
{-# INLINE newModuleState #-}
newModuleState = State UnknownMod [] []
    emptyTable [] Nothing . newPosition


newState :: State
{-# INLINE newState #-}
newState = newModuleState ""
