{-# LANGUAGE Rank2Types #-}

module Semantics.Visitor where

import Control.Monad.Fail (MonadFail(..), fail)
import qualified Data.Map.Strict as Map

import Parser.Data (
    Purity(..),
    Mutability,
    Type(..),
    Value,
    Visibility(..),
    Variable(..),
    )
import Semantics.SymbolTable
import Semantics.Error (Error(..), mergeErrors)



data State
    = State {
        stateTable :: !SymbolTable,
        stateModule :: String,
        statePurity :: Maybe Purity,
        stateExpTypes :: [Type],
        stateFuncName :: String
    }


newtype Visitor a = Visitor {
    runVisitor :: forall b .
        State
        -> (a -> State -> Error -> b)
        -> (SymbolTable -> Error -> b)
        -> b
    }


data Reply a
    = Okay a !SymbolTable !Error
    | Error !SymbolTable !Error



visit :: String -> Visitor a -> Reply a
visit modName (Visitor v) = v state okay err
    where
        state = State newTable modName Nothing [] []
        okay a s e = Okay a (stateTable s) e
        err st e = Error st e


loadTable :: SymbolTable -> Visitor ()
loadTable mt = do
    st <- symbolTable
    let newTypes = Map.union
            (stTypes st)
            (filterInternals (stTypes mt))
    -- let newTraits = Map.union
    --         (stTraits st)
    --         (filterInternals (stTraits mt))
    let newGlobals = Map.union
            (stGlobals st)
            (filterInternals (stGlobals mt))
    setTable (st {
            stTypes = newTypes,
            -- stTraits = newTraits,
            stGlobals = newGlobals,
            stScopeds = [Map.empty]
        })


visitorModule :: Visitor String
visitorModule = Visitor $ \s okay _ ->
    okay (stateModule s) s UnknownError


setCurrentFunc :: String -> Visitor ()
setCurrentFunc name = Visitor $ \s okay _ ->
    okay () (s { stateFuncName = name }) UnknownError


expectedType :: Visitor (Maybe Type)
expectedType = Visitor $ \s okay _ -> case stateExpTypes s of
    [] -> okay Nothing s UnknownError
    (typ:_) -> okay (Just typ) s UnknownError

pushExpType :: Type -> Visitor ()
pushExpType typ = Visitor $ \s okay _ ->
    okay () (s { stateExpTypes = (typ:stateExpTypes s) }) UnknownError

popExpType :: Visitor (Maybe Type)
popExpType = Visitor $ \s okay _ -> case stateExpTypes s of
    [] -> okay Nothing s UnknownError
    (typ:typs) -> okay
        (Just typ) (s { stateExpTypes = typs }) UnknownError


setTable :: SymbolTable -> Visitor ()
setTable st = Visitor $ \s okay _ ->
    okay () (s { stateTable = st }) UnknownError


modifyTable :: (SymbolTable -> SymbolTable) -> Visitor ()
modifyTable f = Visitor $ \s okay _ ->
    okay () (s { stateTable = f (stateTable s) }) UnknownError


symbolData :: Variable -> Visitor SymbolData
symbolData sym = Visitor $ \s okay err ->
    let st = stateTable s in
    case lookupSymbol st sym of
        Found dta -> okay dta s UnknownError
        NotFound e -> case stateExpTypes s of
            [] -> err st e
            (typ:_) ->
                let pur = Impure -- statePurity s
                    dta = Symbol pur Export typ sym ""
                    st' = addUndefSym sym dta st
                in okay dta (s { stateTable = st' })
                    UnknownError

globalData :: Variable -> Visitor SymbolData
globalData sym = Visitor $ \s okay err ->
    let st = stateTable s in
    case lookupGlobal st sym of
        Found dta -> okay dta s UnknownError
        NotFound e -> case stateExpTypes s of
            [] -> err st e
            (typ:_) ->
                let pur = Impure -- statePurity s
                    dta = Symbol pur Export typ sym ""
                    st' = addUndefSym sym dta st
                in okay dta (s { stateTable = st' })
                    UnknownError

typeData :: Variable -> Visitor SymbolData
typeData name = Visitor $ \s okay err ->
    let st = stateTable s in
    case lookupTypename st name of
        Found dta -> okay dta s UnknownError
        NotFound e -> err st e


matchSymToType :: Variable -> Type -> Visitor SymbolData
matchSymToType sym typ = do
    st <- symbolTable
    case lookupSymbol st sym of
        Found dta -> let symType = sdType dta in
            if symType == typ then
                return dta
            else
                typeMismatch typ symType
        NotFound err -> do
            addError err
            pushUndefSym sym


pushScope :: Visitor ()
pushScope = modifyTable addScope

popScope :: Visitor ()
popScope = modifyTable removeScope


withType :: Type -> Visitor a -> Visitor a
withType typ visitor = do
    pushExpType typ
    a <- visitor
    popExpType
    return a


inNewScope :: Visitor a -> Visitor a
inNewScope visitor = do
    pushScope
    a <- visitor
    popScope
    return a


setPurity :: Maybe Purity -> Visitor ()
setPurity pur = Visitor $ \s okay _ ->
    let s' = s { statePurity = pur }
    in okay () s' UnknownError


pushScoped :: Variable -> Mutability -> Type -> Visitor SymbolData
pushScoped sym mut typ = do
    modName <- visitorModule
    let dta = Symbol mut Intern typ sym modName
    modifyTable $! addScoped sym dta
    return $! dta

pushGlobal :: Variable -> SymbolData -> Visitor ()
pushGlobal sym dta = do
    modName <- visitorModule
    let dta' = dta { sdModule = modName }
    modifyTable $! addGlobal sym dta'

pushType :: Variable -> SymbolData -> Visitor ()
pushType name dta = do
    modName <- visitorModule
    let dta' = dta { sdModule = modName }
    modifyTable $! addType name dta'

pushUndefSym :: Variable -> Visitor SymbolData
pushUndefSym sym = do
    expTypes <- expectedType
    case expTypes of
        Nothing -> indetType sym
        Just typ ->
            let pur = Impure -- statePurity s
                dta = Symbol pur Export typ sym ""
            in do
                modifyTable (addUndefSym sym dta)
                return $! dta

pushUndefType :: Variable -> Type -> Visitor SymbolData
pushUndefType sym typ = do
    let pur = Impure -- statePurity s
    let dta = Symbol pur Export typ sym ""
    modifyTable (addUndefSym sym dta)
    return $! dta


symbolTable :: Visitor SymbolTable
symbolTable = Visitor $ \s okay _ ->
    okay (stateTable s) s UnknownError


addError :: Error -> Visitor ()
addError err = Visitor $ \s okay _ -> okay () s err

throw :: Error -> Visitor a
throw err' = Visitor $ \s _ err -> err (stateTable s) err'

typeMismatch :: Type -> Type -> Visitor a
typeMismatch t1 t2 = Visitor $ \s _ err ->
    err (stateTable s) (TypeMismatch t1 t2)

redefinition :: Variable -> Variable -> Visitor a
redefinition orig new = Visitor $ \s _ err ->
    err (stateTable s) (Redefinition orig new)

outOfScope :: Variable -> Visitor a
outOfScope sym = Visitor $ \s _ err ->
    err (stateTable s) (OutOfScope sym)

indetType :: Variable -> Visitor a
indetType sym = Visitor $ \s _ err ->
    err (stateTable s) (IndetType sym)

tooManyArgs :: Variable -> Type -> [Value] -> Visitor a
tooManyArgs func typ vals = Visitor $ \s _ err ->
    err (stateTable s) (TooManyArgs func typ vals)

tooManyParams :: Variable -> Type -> [Variable] -> Visitor a
tooManyParams func typ pars = Visitor $ \s _ err ->
    err (stateTable s) (TooManyParams func typ pars)

otherError :: String -> Visitor a
otherError msg = Visitor $ \s _ err ->
    err (stateTable s) (OtherError msg)

unknownError :: Visitor a
unknownError = Visitor $ \s _ err ->
    err (stateTable s) UnknownError



instance Functor Visitor where
    fmap f (Visitor v) = Visitor $ \s okay err ->
        v s (okay . f) err


instance Applicative Visitor where
    pure a = Visitor $ \s okay _ -> okay a s UnknownError
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
            kerr st err' = verr st (mergeErrors err err')
            in (runVisitor (k x)) s' kokay kerr
        in (runVisitor m) s mokay verr


instance MonadFail Visitor where
    fail = otherError
