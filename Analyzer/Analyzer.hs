{-# LANGUAGE Rank2Types #-}

module Analyzer.Analyzer (
    Control.Monad.Fail.fail,
    Analyzer,
    Analysis(..),
    analyze_,
    getTable, setTable, modifyTable, modifyTable_,
    getModuleName,
    pushScope, popScope,
    peekExpType, expectIn,
    define,
    addImport,
    updatePos,
    option, optional,
    throw, warn, catch, throwUndefined
) where

import Control.Monad ((<$!>))
import Control.Monad.Fail

import Analyzer.Error
import Analyzer.State
import Parser.Components.Imports (Import)
import Parser.Data (Type(..), Var(..))
import SymbolTable


default (Int, Double)


data Analyzer a
    = Analyzer {
        runA :: forall b. State
            -> (a -> State -> b)     -- analyzed
            -> (Error -> State -> b) -- error
            -> b
    }

data Analysis
    = Analysis {
        arErrors :: ![ErrorMessage],
        arTable :: !SymbolTable,
        arImports :: ![Import]
    }


analyze_ :: Analyzer a -> Analysis
analyze_ a = runA a newState okay err
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
{-# INLINE getState #-}
getState = modifyState id

modifyState :: (State -> State) -> Analyzer State
{-# INLINE modifyState #-}
modifyState f = Analyzer $ \ s okay _ ->
    let !s' = f s in okay s' s'

modifyState_ :: (State -> State) -> Analyzer ()
{-# INLINE modifyState_ #-}
modifyState_ f = Analyzer $ \ s okay _ ->
    let !s' = f s in okay () s'

getTable :: Analyzer SymbolTable
{-# INLINE getTable #-}
getTable = stTable <$!> getState

setTable :: SymbolTable -> Analyzer SymbolTable
{-# INLINE setTable #-}
setTable = modifyTable . const

modifyTable :: (SymbolTable -> SymbolTable) -> Analyzer SymbolTable
{-# INLINE modifyTable #-}
modifyTable f = do
    tbl <- f <$!> getTable
    modifyState (\s -> s { stTable = tbl })
    return tbl

modifyTable_ :: (SymbolTable -> SymbolTable) -> Analyzer ()
{-# INLINE modifyTable_ #-}
modifyTable_ f = do
    tbl <- f <$!> getTable
    modifyState (\s -> s { stTable = tbl })
    return ()

getModuleName :: Analyzer Var
{-# INLINE getModuleName #-}
getModuleName = stModule <$!> getState

enterDef :: Symbol -> Analyzer ()
{-# INLINE enterDef #-}
enterDef name = modifyState_ (\s -> s { stDefName = Just name })

exitDef :: Analyzer ()
{-# INLINE exitDef #-}
exitDef = modifyState_ (\s -> s { stDefName = Nothing })

pushScope :: Analyzer ()
{-# INLINE pushScope #-}
pushScope = modifyTable_ $ \tbl ->
    tbl { tblScopeds = (empty:tblScopeds tbl) }

popScope :: Analyzer ()
{-# INLINE popScope #-}
popScope = modifyTable_ $ \tbl ->
    tbl { tblScopeds = case tblScopeds tbl of
            [] -> []
            (_:scps) -> scps
        }

pushExpType :: Type -> Analyzer ()
{-# INLINE pushExpType #-}
pushExpType typ = Analyzer $ \ !s okay _ ->
    okay () (s { stExpType = (typ:stExpType s) })

popExpType :: Analyzer Type
{-# INLINE popExpType #-}
popExpType = Analyzer $ \ !s okay _ -> case stExpType s of
    [] -> okay NoType s
    (typ:rest) -> okay typ (s { stExpType = rest })

peekExpType :: Analyzer Type
{-# INLINE peekExpType #-}
peekExpType = Analyzer $ \ !s okay _ ->
    let typ = case stExpType s of
            [] -> NoType
            (typ':_) -> typ'
    in okay typ s

expectIn :: Type -> Analyzer a -> Analyzer a
{-# INLINE expectIn #-}
expectIn t a = do
    pushExpType t
    x <- a
    popExpType
    return x

define :: Symbol -> Analyzer a -> Analyzer Type
{-# INLINABLE define #-}
define !name analyzer = do
    updatePos $ varPos name
    enterDef name
    analyzer
    exitDef
    return NoType

updatePos :: Position -> Analyzer ()
{-# INLINE updatePos #-}
updatePos p = modifyState_ $ \s -> s { stPosition = p }

addImport :: Import -> Analyzer ()
{-# INLINE addImport #-}
addImport imp = modifyState_ $ \s ->
    s { stImports = (imp:stImports s) }

option :: a -> Analyzer a -> Analyzer a
{-# INLINE option #-}
option def a = catch a >>= return . either (const def) id

optional :: Analyzer a -> Analyzer ()
{-# INLINE optional #-}
optional a = catch a >> return ()

throw :: Error -> Analyzer a
{-# INLINE throw #-}
throw FalseError = Analyzer $ \ !s _ err -> err FalseError s
throw e = Analyzer $ \ s _ err ->
    let es = stErrors s
        em = ErrorMessage {
                emPosition = stPosition s,
                emDefName = stDefName s,
                emError = Right e
            }
    in err e (s { stErrors = (em:es) })

warn :: Warning -> Analyzer ()
{-# INLINE warn #-}
warn w = modifyState_ $ \s -> s { stErrors = ((ErrorMessage {
        emPosition = stPosition s,
        emDefName = stDefName s,
        emError = Left w
    }):stErrors s) }

catch :: Analyzer a -> Analyzer (Either Error a)
{-# INLINE catch #-}
catch a = Analyzer $ \s okay _ ->
    runA a s (okay . Right) (okay . Left)

throwUndefined :: Symbol -> Analyzer a
{-# INLINE throwUndefined #-}
throwUndefined sym = do
    syms <- getSimilarSymbols sym <$!> getTable
    throw $ Undefined sym syms


instance Functor Analyzer where
    {-# INLINE fmap #-}
    fmap f a = Analyzer $ \ !s aok err ->
        let okay x s' = aok (f x) s' in
        runA a s okay err

instance Applicative Analyzer where
    {-# INLINE pure #-}
    pure a = Analyzer $ \ !s okay _ -> okay a s
    {-# INLINE (<*>) #-}
    fa <*> xa = do
        f <- fa
        x <- xa
        return $! f x

instance Monad Analyzer where
    {-# INLINE (>>=) #-}
    a >>= f = Analyzer $ \ !s aok err  ->
        let okay x s' = runA (f x) s' aok err
        in runA a s okay err

instance MonadFail Analyzer where
    {-# INLINE fail #-}
    fail = throw . OtherError
