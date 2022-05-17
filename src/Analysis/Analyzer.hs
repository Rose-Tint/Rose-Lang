{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Analysis.Analyzer (
    Analyzer,
    Warnings(..),
    Flags(..),

    allowBreak,
    purity,
    table,

    newState,
    runAnalyzer,

    askW,
    askF,
    modify,
    modifyTable,
    gets,
    pushScope,
    popScope,
    inNewScope,
    updatePos,

    throw,
    otherError,
    throwUndefined,
    warn,
) where

import Prelude

import Control.Monad ((<$!>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS

import Analysis.Error
import Cmd (
    CmdLine,
    Warnings(..),
    Flags(..),
    warnings,
    flags,
    )
import Common.SrcPos
import Common.Specifiers
import Common.Var
import Data.Table
import Data.VarMap (empty)


default (Int, Double)


data AnState = AnState {
        allowBreak :: Bool,
        purity :: Purity,
        table :: Table,
        stPos :: SrcPos
    }

type Analyzer = RWS
    CmdLine -- for flags and warnings
    [ErrInfo]
    AnState


newState :: AnState
newState = AnState False Pure emptyTable newSrcPos

runAnalyzer :: CmdLine -> Analyzer a -> (a, Table, [ErrInfo])
runAnalyzer cmd an =
    let (a, st, errs) = runRWS an cmd newState
    in (a, table st, errs)

modifyTable :: (Table -> Table) -> Analyzer ()
modifyTable f = modify $ \s -> s { table = f (table s) }

pushScope :: Analyzer ()
pushScope = modifyTable $ \tbl ->
    tbl { tblScopeds = (empty:tblScopeds tbl) }

popScope :: Analyzer ()
popScope = modifyTable $ \tbl -> tbl {
    tblScopeds = case tblScopeds tbl of
        [] -> []
        (_:scps) -> scps
    }

askW :: (Warnings -> Bool) -> Analyzer Bool
askW w = w <$!> asks warnings

askF :: (Flags -> Bool) -> Analyzer Bool
askF f = f <$!> asks flags

inNewScope :: Analyzer a -> Analyzer a
inNewScope an = do
    pushScope
    x <- an
    popScope
    return $! x

updatePos :: HasSrcPos a => a -> Analyzer ()
updatePos p = case getPos p of
    UnknownPos -> return ()
    pos -> modify $ \s -> s { stPos = pos }

throw :: Error -> Analyzer ()
throw FalseError = return ()
throw e = do
    pos <- gets stPos
    tell [ErrInfo pos (Right e)]

warn :: Warning -> Analyzer ()
warn w = do
    pos <- gets stPos
    tell [ErrInfo pos (Left w)]

otherError :: String -> Analyzer ()
otherError = throw . OtherError

throwUndefined :: Var -> Analyzer ()
throwUndefined sym = do
    syms <- gets (getSimilarVars sym . table)
    throw $ Undefined sym syms
