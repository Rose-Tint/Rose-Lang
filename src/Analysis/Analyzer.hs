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
import Control.Monad.Fail

import Analysis.Error
import Common.SrcPos
import Common.Specifiers
import Common.Var
import Data.Table


default (Int, Double)


data State
    = State {
        stModule :: {-# UNPACK #-} !Var,
        stAllowBreak :: Bool,
        stPurity :: Purity,
        stErrors :: [ErrInfo],
        stTable :: Table,
        stDefs :: [Var],
        stPos :: SrcPos
    }

newtype Analyzer a
    = Analyzer {
        runA :: forall b. State
            -> (a -> State -> b)     -- analyzed
            -> (Error -> State -> b) -- error
            -> b
    }

data Analysis
    = Analysis {
        arErrors :: ![ErrInfo],
        arTable :: Table
    }


newState :: String -> State
{-# INLINE newState #-}
newState name = State (prim name)
    False Pure [] emptyTable [] newSrcPos

runAnalyzer :: String -> Analyzer a -> Analysis
runAnalyzer mdl a = runA a (newState mdl) go go
    where
        go _ s = Analysis {
                arErrors = stErrors s,
                arTable = stTable s
            }

getState :: Analyzer State
getState = modifyState id

modifyState :: (State -> State) -> Analyzer State
modifyState f = Analyzer $ \ s okay _ ->
    let !s' = f s in okay s' s'

modifyState_ :: (State -> State) -> Analyzer ()
modifyState_ f = Analyzer $ \ s okay _ ->
    let !s' = f s in okay () s'

getTable :: Analyzer Table
getTable = stTable <$!> getState

setTable :: Table -> Analyzer Table
setTable = modifyTable . const

modifyTable :: (Table -> Table) -> Analyzer Table
modifyTable f = do
    tbl <- f <$!> getTable
    modifyState (\s -> s { stTable = tbl })
    return tbl

modifyTable_ :: (Table -> Table) -> Analyzer ()
modifyTable_ f = do
    modifyState (\s -> s { stTable = f (stTable s) })
    return ()

getModuleName :: Analyzer Var
getModuleName = stModule <$!> getState

enterDef :: Var -> Analyzer ()
enterDef name = modifyState_ (\s -> s { stDefs = (name:stDefs s) })

getCurrDef :: Analyzer Var
getCurrDef = getDefs >>= \case
    [] -> fail "getCurrDef': not in definition"
    (def:_) -> return def

getDefs :: Analyzer [Var]
getDefs = stDefs <$!> getState

exitDef :: Analyzer ()
exitDef = modifyState_  $ \s -> s {
    stDefs = case stDefs s of
        [] -> []
        (_:defs) -> defs
    }

pushScope :: Analyzer ()
pushScope = modifyTable_ $ \tbl ->
    tbl { tblScopeds = (empty:tblScopeds tbl) }

popScope :: Analyzer ()
popScope = modifyTable_ $ \tbl -> tbl {
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

define :: Var -> Analyzer a -> Analyzer a
define !name analyzer = do
    updatePos name
    enterDef name
    x <- analyzer
    exitDef
    return x

updatePos :: HasSrcPos a => a -> Analyzer ()
updatePos p = case getPos p of
    UnknownPos -> return ()
    pos -> modifyState_ $ \s -> s { stPos = pos }

throw :: Error -> Analyzer a
throw FalseError = Analyzer $ \ !s _ err -> err FalseError s
throw e = Analyzer $ \ s _ err ->
    let es = stErrors s
        em = ErrInfo {
                emPos = e <?> stPos s,
                emError = Right e
            }
    in err e (s { stErrors = (em:es) })

warn :: Warning -> Analyzer ()
warn w = modifyState_ $ \s -> s { stErrors = ((ErrInfo {
        emPos = stPos s,
        emError = Left w
    }):stErrors s) }

catch :: Analyzer a -> Analyzer (Either Error a)
catch a = Analyzer $ \s okay _ ->
    runA a s (okay . Right) (okay . Left)

throwUndefined :: Var -> Analyzer a
throwUndefined sym = do
    syms <- getSimilarVars sym <$!> getTable
    throw $ Undefined sym syms


instance Functor Analyzer where
    fmap f a = Analyzer $ \ !s aok err ->
        let okay x = aok (f x) in
        runA a s okay err

instance Applicative Analyzer where
    pure a = Analyzer $ \ !s okay _ -> okay a s
    fa <*> xa = do
        f <- fa
        x <- xa
        return $! f x

instance Monad Analyzer where
    a >>= f = Analyzer $ \ !s aok err  ->
        let okay x s' = runA (f x) s' aok err
        in runA a s okay err

instance MonadFail Analyzer where
    fail = throw . OtherError
