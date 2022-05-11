{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Middle.Analyzer.Internal (
    Control.Monad.Fail.fail,
    Analyzer,
    Analysis(..),
    State(..),
    runAnalyzer,
    -- modifyState, modifyState_,
    getState,
    fresh,
    getTable, setTable, modifyTable, modifyTable_,
    getModuleName,
    pushScope, popScope, inNewScope,
    getTopDef, getCurrDef, getCurrDef', getDefs,
    define,
    updatePos, updatePosVar, updatePosVal,
    option, optional,
    throw, warn, catch, throwUndefined,
) where

import Prelude hiding (fail)

import Control.Monad ((<$!>), void, replicateM)
import Control.Monad.Fail
import Data.Either (fromRight)
import Data.Functor ((<&>))

import Common.SrcPos
import Common.Typing
import Common.Var
import Front.Parser (Value, valPos)
import Middle.Analyzer.Error
import Middle.Analyzer.State
import Middle.Table


default (Int, Double)


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
        arTable :: !Table
    }


runAnalyzer :: Analyzer a -> Analysis
runAnalyzer a = runA a newState okay err
    where
        err _ s = Analysis {
                    arErrors = stErrors s,
                    arTable = stTable s
                }
        okay _ s = Analysis {
                    arErrors = [],
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

getTopDef :: Analyzer Var
getTopDef = last . stDefs <$!> getState

getCurrDef :: Analyzer Var
getCurrDef = getDefs >>= \case
    [] -> getModuleName
    (def:_) -> return def

getCurrDef' :: Analyzer Var
getCurrDef' = getDefs >>= \case
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

fresh :: Analyzer Type
fresh = do
    state <- modifyState $ \s ->
        s { stFreshIdx = stFreshIdx s + 1 }
    let i = stFreshIdx state
    return (TypeVar (prim (letters !! i)))
    where
    letters = [1..] >>= flip replicateM ['a'..'z']

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
    updatePosVar name
    enterDef name
    x <- analyzer
    exitDef
    return x

updatePos :: SrcPos -> Analyzer ()
updatePos UnknownPos = return ()
updatePos p = modifyState_ $ \s -> s { stPos = p }

updatePosVar :: Var -> Analyzer ()
updatePosVar (Var _ p) = updatePos p

updatePosVal :: Value -> Analyzer ()
updatePosVal val = updatePos (valPos val)

option :: a -> Analyzer a -> Analyzer a
option def a = catch a <&> fromRight def

optional :: Analyzer a -> Analyzer ()
optional = void . catch

throw :: Error -> Analyzer a
throw FalseError = Analyzer $ \ !s _ err -> err FalseError s
throw e = Analyzer $ \ s _ err ->
    let es = stErrors s
        em = ErrInfo {
                emPos = stPos s,
                emDefName = Just (head (stDefs s)),
                emError = Right e
            }
    in err e (s { stErrors = (em:es) })

warn :: Warning -> Analyzer ()
warn w = modifyState_ $ \s -> s { stErrors = ((ErrInfo {
        emPos = stPos s,
        emDefName = Just (head (stDefs s)),
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
