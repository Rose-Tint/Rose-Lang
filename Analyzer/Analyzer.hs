{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE BangPatterns #-}

module Analyzer.Analyzer (
    Analyzer,
    Analysis(..),
    analyze,
    getTable, setTable, modifyTable,
    getModuleName,
    pushScope, popScope,
    pushExpType, popExpType, peekExpType, withExpType,
    define,
    addImport,
    updatePos,
    option, optional,
    throw, warn, catch,
    throwUndefined
) where

import Control.Monad ((<$!>))
import Control.Monad.Fail

import Analyzer.Error
import Analyzer.State
import Parser.Data (Module(..), Visibility, Variable(..))
import SymbolTable
import Typing.Types
import Utils ((.!))


default (Int, Double)



data Analyzer a
    = Analyzer {
        runA :: forall b .
            State
            -> (a -> State -> b)     -- analyzed
            -> (Error -> State -> b) -- error
            -> b
    }


data Analysis a
    = Analysis {
        arResult :: !(Maybe a),
        arErrors :: ![ErrorMessage],
        arTable :: !SymbolTable,
        arImports :: ![Module]
    }
    deriving (Show)



analyze :: Analyzer a -> Analysis a
analyze a = runA a newState okay err
    where
        err _ s = Analysis {
                    arResult = Nothing,
                    arErrors = stErrors s,
                    arTable = stTable s,
                    arImports = stImports s
                }
        okay x s = (err FalseError s) { arResult = Just x }


-- getState :: Analyzer State
-- {-# INLINE getState #-}
-- getState = Analyzer $ \ !s okay _ -> okay s s


-- setState :: State -> Analyzer ()
-- {-# INLINE setState #-}
-- setState !s = Analyzer $ \ _ okay _ -> okay () s


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


getModuleName :: Analyzer Module
{-# INLINE getModuleName #-}
getModuleName = Analyzer $ \ !s okay _ ->
    okay (stModule s) s


enterDefinition :: Symbol -> Analyzer ()
{-# INLINABLE enterDefinition #-}
enterDefinition name = Analyzer $ \ !s okay _ ->
    okay () (s { stDefName = Just name })


exitDefinition :: Analyzer ()
{-# INLINABLE exitDefinition #-}
exitDefinition = Analyzer $ \ !s okay _ ->
    okay () (s { stDefName = Nothing })


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


popExpType :: Analyzer Type
{-# INLINABLE popExpType #-}
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


define :: Symbol -> Analyzer a -> Analyzer Type
{-# INLINABLE define #-}
define name analyzer = do
    updatePos $! varPos name
    enterDefinition name
    analyzer
    exitDefinition
    return NoType


withExpType :: Type -> Analyzer a -> Analyzer a
{-# INLINE withExpType #-}
withExpType t a = do
    pushExpType t
    x <- a
    popExpType
    return x


updatePos :: Position -> Analyzer ()
{-# INLINE updatePos #-}
updatePos p = Analyzer $ \ !s okay _ ->
    okay () (s { stPosition = p })


addImport :: Visibility -> Variable -> Analyzer ()
{-# INLINE addImport #-}
addImport vis var = Analyzer $ \ !s okay _ ->
    okay () (s { stImports = (Module vis var:stImports s) })


option :: a -> Analyzer a -> Analyzer a
{-# INLINABLE option #-}
option def a = catch a >>= \x -> return $ case x of
    Left _ -> def
    Right y -> y


optional :: Analyzer a -> Analyzer ()
{-# INLINE optional #-}
optional a = do
    catch a
    return ()


throw :: Error -> Analyzer a
{-# INLINABLE throw #-}
throw FalseError = Analyzer $ \ !s _ err ->
    err FalseError s
throw e = Analyzer $ \ !s _ err ->
    let es = stErrors s
        em = ErrorMessage {
                emPosition = stPosition s,
                emDefName = stDefName s,
                emError = Right e
            }
    in err e (s { stErrors = (em:es) })


warn :: Warning -> Analyzer ()
{-# INLINABLE warn #-}
warn w = Analyzer $ \ !s okay _ ->
    let es = stErrors s
        wm = ErrorMessage {
                emPosition = stPosition s,
                emDefName = stDefName s,
                emError = Left w
            }
    in okay () (s { stErrors = (wm:es) })


catch :: Analyzer a -> Analyzer (Either Error a)
{-# INLINE catch #-}
catch a = Analyzer $ \ !s aok _ ->
    let okay x = aok (Right x)
        err e = aok (Left e)
    in runA a s okay err


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
    fail = throw .! OtherError
