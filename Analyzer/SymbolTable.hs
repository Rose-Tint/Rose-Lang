module Analyzer.SymbolTable where

import Control.Monad ((<$!>), unless)

import Analyzer.Analyzer
import Analyzer.Error
-- import Analyzer.State
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
            let dta = undefined sym
            modifyTable (insertGlobal sym dta)
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


pushType :: Symbol -> SymbolData -> Analyzer ()
pushType sym dta = modifyTable (insertType sym dta)


pushTrait :: Symbol -> SymbolData -> Analyzer ()
pushTrait sym dta = modifyTable (insertTrait sym dta)


pushGlobal :: Symbol -> SymbolData -> Analyzer ()
pushGlobal sym dta = modifyTable (insertGlobal sym dta)


pushScoped :: Symbol -> SymbolData -> Analyzer ()
pushScoped sym dta = modifyTable (insertScoped sym dta)
