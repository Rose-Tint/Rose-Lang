{-# LANGUAGE Rank2Types #-}

module Semantics.Visitor where

import Control.Monad.Fail

import Parser.Data
import Semantics.SymbolTable
import Semantics.Error



data State
    = State {
        stateTable :: SymbolTable
    }


newtype Visitor a = Visitor {
    runVisitor :: forall b .
        State
        -> (a -> State -> Error -> b)
        -> (Error -> b)
        -> b
    }



modifyTable :: (SymbolTable -> SymbolTable) -> Visitor ()
modifyTable f = Visitor $ \s okay _ ->
    okay () (s { stateTable = f (stateTable s) }) UnknownError


-- TODO: optimize by using the constructor
symbolData :: String -> Visitor SymbolLookup
symbolData sym = do
    st <- symbolTable
    return $! lookupSymbol st sym


globalData :: String -> Visitor SymbolLookup
globalData sym = do
    st <- symbolTable
    return (lookupGlobal st sym)


typeData :: String -> Visitor SymbolLookup
typeData name = do
    st <- symbolTable
    return (lookupTypename st name)


pushScope :: Visitor ()
pushScope = modifyTable addScope


pushSymbol, pushGlobal, pushType ::
    String -> SymbolData -> Visitor ()
pushSymbol sym dta = modifyTable $! addScoped sym dta
pushGlobal sym dta = modifyTable $! addGlobal sym dta
pushType name dta = modifyTable $! addType name dta


popScope :: Visitor ()
popScope = modifyTable removeScope


symbolTable :: Visitor SymbolTable
symbolTable = Visitor $ \s okay _ ->
    okay (stateTable s) s UnknownError


typeMismatch :: Type -> Type -> Visitor a
typeMismatch t1 t2 = Visitor $ \_ _ err ->
    err (TypeMismatch t1 t2)


outOfScope :: String -> Visitor a
outOfScope sym = Visitor $ \_ _ err ->
    err (OutOfScope sym)


tooManyArgs :: Type -> [Value] -> Visitor a
tooManyArgs typ vals = Visitor $ \_ _ err ->
    err (TooManyArgs typ vals)


otherError :: String -> Visitor a
otherError msg = Visitor $ \_ _ err ->
    err (OtherError msg)


unknownError :: Visitor a
unknownError = Visitor $ \_ _ err ->
    err UnknownError


(<|>) :: Visitor a -> Visitor a -> Visitor a
Visitor lv <|> Visitor rv = Visitor $ \s vok verr -> let
    lerr err = let
            rok y s' err' =
                vok y s' (mergeErrors err err')
            rerr err' = verr (mergeErrors err err')
        in rv s rok rerr
    in lv s vok lerr



instance Functor Visitor where
    fmap f (Visitor v) = Visitor $ \s okay err ->
        v s (okay . f) err


instance Applicative Visitor where
    pure a = Visitor $ \st okay _ -> okay a st UnknownError
    vf <*> vx = do
        f <- vf
        x <- vx
        return (f x)


instance Monad Visitor where
    return = pure
    m >>= k = Visitor $ \s vokay verr -> let
        mokay x s' UnknownError = (runVisitor (k x)) s' vokay verr
        mokay x s' err = let
            kokay x' s'' err' = vokay x' s'' (mergeErrors err err')
            kerr err' = verr (mergeErrors err err')
            in (runVisitor (k x)) s' kokay kerr
        in (runVisitor m) s mokay verr


instance MonadFail Visitor where
    fail = otherError
