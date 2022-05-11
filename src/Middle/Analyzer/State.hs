module Middle.Analyzer.State (
    State(..),
    newModuleState,
    newState,
) where

import Common.SrcPos
import Common.Var
import Middle.Analyzer.Error
import Middle.Table
-- import Middle.Typing.Scheme


data State
    = State {
        stModule :: {-# UNPACK #-} !Var,
        stFreshIdx :: {-# UNPACK #-} !Int,
        -- stTypeEnv :: TypeEnv,
        stAllowBreak :: Bool,
        stErrors :: [ErrInfo],
        stTable :: Table,
        stDefs :: [Var],
        stPos :: SrcPos
    }


newModuleState :: String -> State
{-# INLINE newModuleState #-}
newModuleState name = State
    (prim name) 0 False [] emptyTable [] newSrcPos

newState :: State
{-# INLINE newState #-}
newState = newModuleState ""
