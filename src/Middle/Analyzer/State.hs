module Middle.Analyzer.State (
    State(..),
    newModuleState,
    newState,
) where

import Common.SrcPos
import Common.Typing
import Common.Var
import Middle.Analyzer.Error
import Middle.Table


data State
    = State {
        stModule :: {-# UNPACK #-} !Var,
        stExpType :: [Type],
        stErrors :: [ErrInfo],
        stTable :: Table,
        stDefs :: [Var],
        stPos :: SrcPos
    }


newModuleState :: String -> State
{-# INLINE newModuleState #-}
newModuleState name = State (prim name) [Delayed] []
    emptyTable [] newSrcPos

newState :: State
{-# INLINE newState #-}
newState = newModuleState ""
