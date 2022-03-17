{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE BangPatterns #-}

module Analyzer.Analyzer (
    Analyzer,
    Analysis(..),
    analyze,
    getTable, setTable, modifyTable,
    enterDefinition, exitDefinition, getCurrDef,
    pushFnCall, popFnCall,
    pushScope, popScope,
    pushExpType, popExpType, peekExpType, withExpType,
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


data Analysis a
    = Analysis {
        arResult :: !(Maybe a),
        arErrors :: ![(ErrorInfo, Error)],
        arWarnings :: ![(ErrorInfo, Warning)],
        arTable :: !SymbolTable,
        arImports :: ![Module]
    }
    deriving (Show)



analyze :: CmdLine -> Analyzer a -> Analysis a
analyze cmd a = runA a (newState cmd) okay err
    where
        err s = Analysis {
                    arResult = Nothing,
                    arErrors = stErrors s,
                    arWarnings = stWarnings s,
                    arTable = stTable s,
                    arImports = stImports s
                }
        okay x s = Analysis {
                    arResult = Just x,
                    arErrors = stErrors s,
                    arWarnings = stWarnings s,
                    arTable = stTable s,
                    arImports = stImports s
                }


getTable :: Analyzer SymbolTable
{-# INLINE getTable #-}
getTable = Analyzer $ \ !s okay _ -> okay (stTable s) s


setTable :: SymbolTable -> Analyzer ()
{-# INLINE setTable #-}
setTable tbl = Analyzer $ \ !s okay _ ->
    okay () (s { stTable = tbl })


modifyTable :: (SymbolTable -> SymbolTable) -> Analyzer ()
{-# INLINE modifyTable #-}
modifyTable f = Analyzer $ \ !s okay _ ->
    okay () (s { stTable = f (stTable s) })


fromCmdLine :: (CmdLine -> a) -> Analyzer a
{-# INLINE fromCmdLine #-}
fromCmdLine f = Analyzer $ \ !s okay _ ->
    okay (f (stCmdLine s)) s


enterDefinition :: Symbol -> Analyzer ()
{-# INLINABLE enterDefinition #-}
enterDefinition name = Analyzer $ \ !s okay _ ->
    let errInfo = (stErrInfo s) { eiDefName = Just name }
    in okay () (s { stErrInfo = errInfo })


exitDefinition :: Analyzer ()
{-# INLINABLE exitDefinition #-}
exitDefinition = Analyzer $ \ !s okay _ ->
    let errInfo = (stErrInfo s) { eiDefName = Nothing }
    in okay () (s { stErrInfo = errInfo })


getCurrDef :: Analyzer (Maybe Symbol)
{-# INLINE getCurrDef #-}
getCurrDef = Analyzer $ \ !s okay _ ->
    okay (eiDefName (stErrInfo s)) s


pushFnCall :: Symbol -> Analyzer ()
{-# INLINABLE pushFnCall #-}
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
{-# INLINE pushExpType #-}
pushExpType typ = Analyzer $ \ !s okay _ ->
    okay () (s { stExpType = (typ:stExpType s) })


popExpType :: Analyzer (Maybe Type)
{-# INLINABLE popExpType #-}
popExpType = Analyzer $ \ !s okay _ -> case stExpType s of
    [] -> okay Nothing s
    (typ:rest) -> okay (Just typ) (s { stExpType = rest })


peekExpType :: Analyzer (Maybe Type)
{-# INLINE peekExpType #-}
peekExpType = Analyzer $ \ !s okay _ ->
    okay (listToMaybe (stExpType s)) s


withExpType :: Type -> Analyzer a -> Analyzer a
withExpType t a = do
    pushExpType t
    x <- a
    popExpType
    return $! x


addImport :: Visibility -> Variable -> Analyzer ()
{-# INLINE addImport #-}
addImport vis var = Analyzer $ \ !s okay _ ->
    okay () (s { stImports = (Module vis var:stImports s) }) 


throw :: Error -> Analyzer a
{-# INLINABLE throw #-}
throw e = Analyzer $ \ !s _ err ->
    let ei = stErrInfo s
        es = stErrors s
    in err (s { stErrors = ((ei, e):es) })


warn :: Warning -> Analyzer ()
{-# INLINABLE warn #-}
warn w = Analyzer $ \ !s okay _ ->
    let ei = stErrInfo s
        ws = stWarnings s
    in okay () (s { stWarnings = ((ei, w):ws) })


catch :: Analyzer a -> Analyzer (Maybe a)
{-# INLINE catch #-}
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
