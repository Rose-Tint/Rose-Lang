module Middle.Analyzer.State (
    State(..),
    newModuleState,
    newState,
) where

import Common.SrcPos
import Common.Typing
import Common.Var
import Front.Parser (Import)
import Middle.Analyzer.Error
import Middle.SymbolTable


data State
    = State {
        stModule :: {-# UNPACK #-} !Var,
        stExpType :: [Type],
        stErrors :: [ErrInfo],
        stTable :: SymbolTable,
        stImports :: [Import],
        stDefName :: Maybe Symbol,
        stPos :: SrcPos
    }


newModuleState :: String -> State
{-# INLINE newModuleState #-}
newModuleState name = State (prim name) [] []
    emptyTable [] Nothing newSrcPos

newState :: State
{-# INLINE newState #-}
newState = newModuleState ""
