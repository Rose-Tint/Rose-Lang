{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE BangPatterns #-}

module Analyzer.Analyzer (
    Analyzer,
    getTable, setTable, modifyTable,
    enterDefinition, exitDefinition, getCurrDef,
    pushFnCall, popFnCall,
    pushScope, popScope,
    pushExpType, popExpType, peekExpType,
    fromCmdLine,
    addImport,
    throw, warn, catch,
) where

import Control.Monad.Fail
import Data.Maybe (listToMaybe)

import Analyzer.Error
import Analyzer.State
import CmdLine (CmdLine)
import Parser.Data (Visibility, Variable)
import SymbolTable
import Typing.Types
import Utils ((.!))



data Analyzer a
    = Analyzer {
        runA :: forall b .
            State
            -> (a -> State -> b) -- analyzed
            -> (State -> b)      -- error
            -> b
    }



getTable :: Analyzer SymbolTable
getTable = Analyzer $ \ !s okay _ -> okay (stTable s) s


setTable :: SymbolTable -> Analyzer ()
setTable tbl = Analyzer $ \ !s okay _ ->
    okay () (s { stTable = tbl })


modifyTable :: (SymbolTable -> SymbolTable) -> Analyzer ()
modifyTable f = Analyzer $ \ !s okay _ ->
    okay () (s { stTable = f (stTable s) })


fromCmdLine :: (CmdLine -> a) -> Analyzer a
fromCmdLine f = Analyzer $ \ !s okay _ ->
    okay (f (stCmdLine s)) s


enterDefinition :: Symbol -> Analyzer ()
enterDefinition name = Analyzer $ \ !s okay _ ->
    let errInfo = (stErrInfo s) { eiDefName = Just name }
    in okay () (s { stErrInfo = errInfo })


exitDefinition :: Analyzer ()
exitDefinition = Analyzer $ \ !s okay _ ->
    let errInfo = (stErrInfo s) { eiDefName = Nothing }
    in okay () (s { stErrInfo = errInfo })


getCurrDef :: Analyzer (Maybe Symbol)
getCurrDef = Analyzer $ \ !s okay _ ->
    okay (eiDefName (stErrInfo s)) s


pushFnCall :: Symbol -> Analyzer ()
pushFnCall name = Analyzer $ \ !s okay _ ->
    let errInfo = (stErrInfo s)
            { eiCalls = (name:eiCalls errInfo) }
    in okay () (s { stErrInfo = errInfo })


popFnCall :: Analyzer ()
popFnCall = Analyzer $ \ !s okay _ ->
    let errInfo = stErrInfo s
    in case eiCalls errInfo of
        [] -> okay () s
        (_:calls) -> okay ()
            (s { stErrInfo = errInfo { eiCalls = calls } })


pushScope, popScope :: Analyzer ()
pushScope = Analyzer $ \ !s okay _ ->
    let tbl = stTable s
        tbl' = tbl { tblScopeds = (empty:tblScopeds tbl) }
        s' = s { stTable = tbl' }
    in okay () s'
popScope = Analyzer $ \ !s okay _ ->
    let tbl = (stTable s) { tblScopeds =
            case tblScopeds (stTable s) of
                [] -> []
                (_:scps) -> scps
            }
        s' = s { stTable = tbl }
    in okay () s'


pushExpType :: Type -> Analyzer ()
pushExpType typ = Analyzer $ \ !s okay _ ->
    okay () (s { stExpType = (typ:stExpType s) })


popExpType :: Analyzer (Maybe Type)
popExpType = Analyzer $ \ !s okay _ -> case stExpType s of
    [] -> okay Nothing s
    (typ:rest) -> okay (Just typ) (s { stExpType = rest })


peekExpType :: Analyzer (Maybe Type)
peekExpType = Analyzer $ \ !s okay _ ->
    okay (listToMaybe (stExpType s)) s


addImport :: Visibility -> Variable -> Analyzer ()
addImport vis var = Analyzer $ \ !s okay _ ->
    okay () (s { stImports = (Module vis var:stImports s) }) 


throw :: Error -> Analyzer a
throw e = Analyzer $ \ !s _ err ->
    let ei = stErrInfo s
        es = stErrors s
    in err (s { stErrors = ((ei, e):es) })


warn :: Warning -> Analyzer ()
warn w = Analyzer $ \ !s okay _ ->
    let ei = stErrInfo s
        ws = stWarnings s
    in okay () (s { stWarnings = ((ei, w):ws) })


catch :: Analyzer a -> Analyzer (Maybe a)
catch a = Analyzer $ \ !s okay _ ->
    runA a s (okay . Just) (okay Nothing)



instance Functor Analyzer where
    fmap f a = Analyzer $ \ !s aok err ->
        let okay x s' = aok (f x) s' in
        runA a s okay err


instance Applicative Analyzer where
    pure a = Analyzer $ \ !s okay _ -> okay a s
    fa <*> xa = do
        f <- fa
        x <- xa
        return $! f x


instance Monad Analyzer where
    return = pure
    a >>= f = Analyzer $ \ !s aok err  ->
        let okay x s' = runA (f x) s' aok err
        in runA a s okay err


instance MonadFail Analyzer where
    fail = throw .! OtherError
