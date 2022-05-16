{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Analysis.Analyzer (
    Analyzer,
    Analysis(..),
    State(..),

    newState,
    runAnalyzer,

    getState,
    modifyState,
    modifyState_,

    getTable,
    setTable,
    modifyTable,
    modifyTable_,

    getModuleName,

    pushScope,
    popScope,
    inNewScope,

    getCurrDef,
    define,

    updatePos,

    fail,
    throw,
    throwUndefined,
    warn,
    catch,
) where

import Prelude hiding (fail)

import Control.Monad ((<$!>))
import Control.Monad.Trans.State
import Control.Monad.Fail

import Analysis.Error
import Common.SrcPos
import Common.Specifiers
import Common.Var
import Data.Table


default (Int, Double)


data AnState = AnState {
        allowBreak :: Bool,
        purity :: Purity,
        table :: Table,
        stPos :: SrcPos
    }

type Analyzer a = WriterT [ErrInfo] (State AnState) a


newState :: AnState
newState name = AnState False emptyTable newSrcPos

runAnalyzer :: Analyzer a -> ([ErrInfo], a)
runAnalyzer an = evalState (runWriterT an) newState

typeEnv :: AnState -> TypeEnv
typeEnv = gets (tblEnv . table)

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
throw FalseError = Analyzer $ \ !s _ err -> err FalseError s
throw e = do
    pos <- gets stPos
    tell [ErrInfo pos (Right e)]

warn :: Warning -> Analyzer ()
warn w = do
    pos <- gets stPos
    tell [ErrInfo pos (Left w)]

throwUndefined :: Var -> Analyzer a
throwUndefined sym = do
    syms <- getSimilarVars sym <$!> getTable
    throw $ Undefined sym syms
