module Analyzer.SymbolTable where

import Control.Monad ((<$!>), unless)

import Analyzer.Analyzer
import Analyzer.Error
import CmdLine (CmdLine(..))
import SymbolTable



-- Thus is the convention for Symbol Table lookups:
--   - `search*` returns a `SymbolData` object. It will
--       also insert an "undefined" symbol into the
--       appropriate table if it is not found.
--   - `lookup*` returns a `Maybe SymbolData` object.
--       `Nothing` if not found, and `Just dta` if found.



searchTypes :: Symbol -> Analyzer SymbolData
searchTypes sym = do
    typs <- tblTypes <$!> getTable
    case search sym typs of
        Nothing -> do
            let dta = undefined sym
            modifyTable (insertType sym dta)
            return dta
        Just dta -> return $! dta


searchTraits :: Symbol -> Analyzer SymbolData
searchTraits sym = do
    trts <- tblTraits <$!> getTable
    case search sym trts of
        Nothing -> do
            let dta = undefined sym
            modifyTable (insertTrait sym dta)
            return dta
        Just dta -> return $! dta


searchGlobals :: Symbol -> Analyzer SymbolData
searchGlobals sym = do
    glbs <- tblGlobals <$!> getTable
    case search sym glbs of
        Nothing -> do
            let dta = undef sym
            expType <- peekExpType
            dta' <- case expType of
                Nothing -> return dta
                Just typ -> return $! dta
                    { sdType = typ }
            modifyTable (insertGlobal sym dta')
            return dta
        Just dta -> return $! dta


searchScopeds :: Symbol -> Analyzer SymbolData
searchScopeds sym = tblScopeds <$!> getTable >>= go >>= \res ->
    case res of
        Nothing -> do
            let dta = undefined sym
            modifyTable (insertGlobal sym dta)
            return dta
        Just dta -> return $! dta
    where
        go [] = Just <$!> searchGlobals sym
        go (scp:scps) = case search sym scp of
            Just dta -> do
                allowShadowing <- fromCmdLine cmdShadowing
                unless allowShadowing $! do
                    rest <- go scps
                    case rest of
                        Nothing -> return ()
                        Just other -> warn
                            (ShadowsName sym (sdVar other))
                return (Just dta)
            Nothing -> go scps



findType :: Symbol -> Analyzer (Maybe SymbolData)
findType sym = do
    typs <- tblTypes <$!> getTable
    return $! search sym typs
findTrait :: Symbol -> Analyzer (Maybe SymbolData)
findTrait sym = do
    trts <- tblTraits <$!> getTable
    return $! search sym trts
findGlobal :: Symbol -> Analyzer (Maybe SymbolData)
findGlobal sym = do
    glbs <- tblGlobals <$!> getTable
    return $! search sym glbs
findScoped :: Symbol -> Analyzer (Maybe SymbolData)
findScoped sym = do
    scps <- tblScopeds <$!> getTable
    go scps
    where
        go :: [SymbolMap] -> Analyzer (Maybe SymbolData)
        go [] = findGlobal sym
        go (scp:scps) = case search sym scp of
            Nothing -> go scps
            Just dta -> return $! Just dta


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
pushType sym dta = modifyTable (insertType sym dta)


pushTrait :: Symbol -> SymbolData -> Analyzer ()
pushTrait sym dta = modifyTable (insertTrait sym dta)


pushGlobal :: Symbol -> SymbolData -> Analyzer ()
pushGlobal sym dta = modifyTable (insertGlobal sym dta)


pushScoped :: Symbol -> SymbolData -> Analyzer ()
pushScoped sym dta = modifyTable (insertScoped sym dta)
