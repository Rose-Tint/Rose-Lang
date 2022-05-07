{-# LANGUAGE Rank2Types #-}

module Middle.Analyzer.Internal (
    Control.Monad.Fail.fail,
    Analyzer,
    Analysis(..),
    runAnalyzer,
    getTable, setTable, modifyTable, modifyTable_,
    getModuleName,
    pushScope, popScope,
    peekExpType, expect, expect',
    define,
    addImport,
    updatePos, updatePosVar,
    option, optional,
    throw, warn, catch, throwUndefined,
) where

import Control.Monad ((<$!>), void)
import Control.Monad.Fail
import Data.Either (fromRight)
import Data.Functor ((<&>))

import Common.SrcPos
import Common.Typing
import Common.Var
import Front.Parser (Import)
import Middle.Analyzer.Error
import Middle.Analyzer.State
import Middle.SymbolTable


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
        arTable :: !SymbolTable,
        arImports :: ![Import]
    }


runAnalyzer :: Analyzer a -> Analysis
runAnalyzer a = runA a newState okay err
    where
        err _ s = Analysis {
                    arErrors = stErrors s,
                    arTable = stTable s,
                    arImports = stImports s
                }
        okay _ s = Analysis {
                    arErrors = [],
                    arTable = stTable s,
                    arImports = stImports s
                }

getState :: Analyzer State
getState = modifyState id

modifyState :: (State -> State) -> Analyzer State
modifyState f = Analyzer $ \ s okay _ ->
    let !s' = f s in okay s' s'

modifyState_ :: (State -> State) -> Analyzer ()
modifyState_ f = Analyzer $ \ s okay _ ->
    let !s' = f s in okay () s'

getTable :: Analyzer SymbolTable
getTable = stTable <$!> getState

setTable :: SymbolTable -> Analyzer SymbolTable
setTable = modifyTable . const

modifyTable :: (SymbolTable -> SymbolTable) -> Analyzer SymbolTable
modifyTable f = do
    tbl <- f <$!> getTable
    modifyState (\s -> s { stTable = tbl })
    return tbl

modifyTable_ :: (SymbolTable -> SymbolTable) -> Analyzer ()
modifyTable_ f = do
    tbl <- f <$!> getTable
    modifyState (\s -> s { stTable = tbl })
    return ()

getModuleName :: Analyzer Var
getModuleName = stModule <$!> getState

enterDef :: Symbol -> Analyzer ()
enterDef name = modifyState_ (\s -> s { stDefName = Just name })

exitDef :: Analyzer ()
exitDef = modifyState_ (\s -> s { stDefName = Nothing })

pushScope :: Analyzer ()
pushScope = modifyTable_ $ \tbl ->
    tbl { tblScopeds = (empty:tblScopeds tbl) }

popScope :: Analyzer ()
popScope = modifyTable_ $ \tbl ->
    tbl { tblScopeds = case tblScopeds tbl of
            [] -> []
            (_:scps) -> scps
        }

pushExpType :: Type -> Analyzer ()
pushExpType NoType = return ()
pushExpType typ = Analyzer $ \ !s okay _ ->
    okay () (s { stExpType = (typ:stExpType s) })

popExpType :: Analyzer Type
popExpType = Analyzer $ \ !s okay _ -> case stExpType s of
    [] -> okay NoType s
    (typ:rest) -> okay typ (s { stExpType = rest })

peekExpType :: Analyzer Type
peekExpType = Analyzer $ \ !s okay _ ->
    let typ = case stExpType s of
            [] -> NoType
            (typ':_) -> typ'
    in okay typ s

expect :: Type -> Analyzer a -> Analyzer a
expect NoType = id
expect t = expect' t

-- `expect'` is 'strict' in the sense that, unlike
-- `expect`, `expect'` allows `NoType`s to be pushed
expect' :: Type -> Analyzer a -> Analyzer a
expect' t a = do
    pushExpType t
    x <- a
    popExpType
    return x

define :: Symbol -> Analyzer a -> Analyzer Type
define !name analyzer = do
    updatePos $ varPos name
    enterDef name
    analyzer
    exitDef
    return NoType

updatePos :: SrcPos -> Analyzer ()
updatePos UnknownPos = return ()
updatePos p = modifyState_ $ \s -> s { stPos = p }

updatePosVar :: Var -> Analyzer ()
updatePosVar (Var _ p) = updatePos p

addImport :: Import -> Analyzer ()
addImport imp = modifyState_ $ \s ->
    s { stImports = (imp:stImports s) }

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
                emDefName = stDefName s,
                emError = Right e
            }
    in err e (s { stErrors = (em:es) })

warn :: Warning -> Analyzer ()
warn w = modifyState_ $ \s -> s { stErrors = ((ErrInfo {
        emPos = stPos s,
        emDefName = stDefName s,
        emError = Left w
    }):stErrors s) }

catch :: Analyzer a -> Analyzer (Either Error a)
catch a = Analyzer $ \s okay _ ->
    runA a s (okay . Right) (okay . Left)

throwUndefined :: Symbol -> Analyzer a
throwUndefined sym = do
    syms <- getSimilarSymbols sym <$!> getTable
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
