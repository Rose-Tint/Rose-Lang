module Middle.Analyzer.SymbolTable (
    searchTypes, searchTraits, searchGlobals, searchScopeds,
    findType, findTrait, findGlobal, findScoped,
    lookupType, lookupTrait, lookupGlobal, lookupScoped,
    modifyType, modifyTrait, modifyGlobal, modifyScoped,
    pushType, pushTrait, pushGlobal, pushScoped,
) where

import Prelude hiding (lookup)

import Control.Monad ((<$!>))

import Common.Typing.Type
import Middle.Analyzer.Internal
import Middle.SymbolTable


-- Thus is the convention for Symbol Table lookups:
--   - `search*` always returns a `SymbolData` object.
--       It will insert an "undefined" symbol into the
--       appropriate table if it is not found.
--   - `lookup*` returns a `Maybe SymbolData` object.
--       `Nothing` if not found, and `Just dta` if found.


searchTypes :: Symbol -> Analyzer SymbolData
searchTypes sym = do
    typs <- tblTypes <$!> getTable
    case lookup sym typs of
        Nothing -> do
            let dta = undef
            modifyTable (insertType sym dta)
            return dta
        Just dta -> return $! dta

searchTraits :: Symbol -> Analyzer SymbolData
searchTraits sym = do
    trts <- tblTraits <$!> getTable
    case lookup sym trts of
        Nothing -> do
            let dta = undef
            modifyTable (insertTrait sym dta)
            return dta
        Just dta -> return $! dta

searchGlobals :: Symbol -> Analyzer SymbolData
searchGlobals sym = do
    glbs <- tblGlobals <$!> getTable
    case lookup sym glbs of
        Nothing -> do
            eT <- peekExpType
            let dta = mkSymbolData sym eT Nothing Nothing
            expType <- peekExpType
            dta' <- case expType of
                NoType -> return dta
                typ -> return $! dta
                    { sdType = typ <::> sdType dta }
            modifyTable (insertGlobal sym dta')
            return dta
        Just dta -> return $! dta

searchScopeds :: Symbol -> Analyzer SymbolData
searchScopeds sym = do
    scps <- tblScopeds <$!> getTable
    go scps
    where
        go [] = searchGlobals sym
        go (scp:scps) = maybe (go scps)
            return (lookup sym scp)

findType :: Symbol -> Analyzer SymbolData
findType sym = lookupType sym >>= maybe
    (throwUndefined sym) return

findTrait :: Symbol -> Analyzer SymbolData
findTrait sym = lookupTrait sym >>= maybe
    (throwUndefined sym) return

findGlobal :: Symbol -> Analyzer SymbolData
findGlobal sym = lookupGlobal sym >>= maybe
    (throwUndefined sym) return

findScoped :: Symbol -> Analyzer SymbolData
findScoped sym = lookupScoped sym >>= maybe
    (throwUndefined sym) return

lookupType :: Symbol -> Analyzer (Maybe SymbolData)
lookupType sym = do
    typs <- tblTypes <$!> getTable
    return $! lookup sym typs

lookupTrait :: Symbol -> Analyzer (Maybe SymbolData)
lookupTrait sym = do
    trts <- tblTraits <$!> getTable
    return $! lookup sym trts

lookupGlobal :: Symbol -> Analyzer (Maybe SymbolData)
lookupGlobal sym = do
    glbs <- tblGlobals <$!> getTable
    return $! lookup sym glbs

lookupScoped :: Symbol -> Analyzer (Maybe SymbolData)
lookupScoped sym = do
    scps <- tblScopeds <$!> getTable
    go scps
    where
        go :: [SymbolMap] -> Analyzer (Maybe SymbolData)
        go [] = lookupGlobal sym
        go (scp:scps) = case lookup sym scp of
            Nothing -> go scps
            Just dta -> return (Just dta)


modifyType :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
modifyType sym f = do
    dta <- searchTypes sym
    pushType sym $! f dta

modifyTrait :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
modifyTrait sym f = do
    dta <- searchTraits sym
    pushTrait sym $! f dta

modifyGlobal :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
modifyGlobal sym f = do
    dta <- searchGlobals sym
    pushGlobal sym $! f dta

modifyScoped :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
modifyScoped sym f = do
    dta <- searchScopeds sym
    pushScoped sym $! f dta

pushType :: Symbol -> SymbolData -> Analyzer ()
pushType sym = modifyTable_ . insertType sym

pushTrait :: Symbol -> SymbolData -> Analyzer ()
pushTrait sym = modifyTable_ . insertTrait sym

pushGlobal :: Symbol -> SymbolData -> Analyzer ()
pushGlobal sym = modifyTable_ . insertGlobal sym

pushScoped :: Symbol -> SymbolData -> Analyzer ()
pushScoped sym = modifyTable_ . insertScoped sym
