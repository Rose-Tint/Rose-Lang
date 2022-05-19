module Typing.Analyzer (

) where

import Control.Monad.Trans


data AnState = AnState {
    jumpAllowed :: Bool,
    purity :: Purity,
    table :: Table,
    position :: SrcPos,
    freshIdx :: Int
    }

type AnalyzeT = StateT AnState

type Analyze = State AnState


mkState :: Table -> AnState
mkState tbl = AnState {
    jumpAllowed = False,
    purity = Pure,
    table = tbl,
    position = newSrcPos,
    freshIdx = 0
    }
