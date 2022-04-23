module Analyzer.SymbolTable (
    searchTypes, searchTraits, searchGlobals, searchScopeds,
    findType, findTrait, findGlobal, findScoped,
    modifyType, modifyTrait, modifyGlobal, modifyScoped,
    pushType, pushTrait, pushGlobal, pushScoped,
) where

import Prelude hiding (lookup)

import Control.Monad ((<$!>))

import Analyzer.Analyzer
import Parser.Data (Type(..))
import SymbolTable


-- Thus is the convention for Symbol Table lookups:
--   - `search*` always returns a `SymbolData` object.
--       It will insert an "undefined" symbol into the
--       appropriate table if it is not found.
--   - `find*` returns a `Maybe SymbolData` object.
--       `Nothing` if not found, and `Just dta` if found.


searchTypes :: Symbol -> Analyzer SymbolData
{-# INLINABLE searchTypes #-}
searchTypes sym = do
    typs <- tblTypes <$!> getTable
    case lookup sym typs of
        Nothing -> do
            let dta = undefined sym
            modifyTable (insertType sym dta)
            return dta
        Just dta -> return $! dta

searchTraits :: Symbol -> Analyzer SymbolData
{-# INLINABLE searchTraits #-}
searchTraits sym = do
    trts <- tblTraits <$!> getTable
    case lookup sym trts of
        Nothing -> do
            let dta = undefined sym
            modifyTable (insertTrait sym dta)
            return dta
        Just dta -> return $! dta

searchGlobals :: Symbol -> Analyzer SymbolData
{-# INLINABLE searchGlobals #-}
searchGlobals sym = do
    glbs <- tblGlobals <$!> getTable
    case lookup sym glbs of
        Nothing -> do
            eT <- peekExpType
            let dta = mkSymbolData sym eT Nothing Nothing
            expType <- peekExpType
            dta' <- case expType of
                NoType -> return dta
                typ -> return $! dta { sdType = typ }
            modifyTable (insertGlobal sym dta')
            return dta
        Just dta -> return $! dta

searchScopeds :: Symbol -> Analyzer SymbolData
{-# INLINE searchScopeds #-}
searchScopeds sym = do
    scps <- tblScopeds <$!> getTable
    go scps
    where
        go [] = searchGlobals sym
        go (scp:scps) = maybe
            (go scps) return (lookup sym scp)

findType :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINE findType #-}
findType sym = do
    typs <- tblTypes <$!> getTable
    return $! lookup sym typs

findTrait :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINE findTrait #-}
findTrait sym = do
    trts <- tblTraits <$!> getTable
    return $! lookup sym trts

findGlobal :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINE findGlobal #-}
findGlobal sym = do
    glbs <- tblGlobals <$!> getTable
    return $! lookup sym glbs

findScoped :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINE findScoped #-}
findScoped sym = do
    scps <- tblScopeds <$!> getTable
    go scps
    where
        go :: [SymbolMap] -> Analyzer (Maybe SymbolData)
        go [] = findGlobal sym
        go (scp:scps) = case lookup sym scp of
            Nothing -> go scps
            Just dta -> return $! Just dta

modifyType :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
{-# INLINE modifyType #-}
modifyType sym f = do
    dta <- searchTypes sym
    pushType sym $! f dta

modifyTrait :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
{-# INLINE modifyTrait #-}
modifyTrait sym f = do
    dta <- searchTraits sym
    pushTrait sym $! f dta

modifyGlobal :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
{-# INLINE modifyGlobal #-}
modifyGlobal sym f = do
    dta <- searchGlobals sym
    pushGlobal sym $! f dta

modifyScoped :: Symbol -> (SymbolData -> SymbolData) -> Analyzer ()
{-# INLINE modifyScoped #-}
modifyScoped sym f = do
    dta <- searchScopeds sym
    pushScoped sym $! f dta

pushType :: Symbol -> SymbolData -> Analyzer ()
{-# INLINE pushType #-}
pushType sym = modifyTable_ . insertType sym

pushTrait :: Symbol -> SymbolData -> Analyzer ()
{-# INLINE pushTrait #-}
pushTrait sym = modifyTable_ . insertTrait sym

pushGlobal :: Symbol -> SymbolData -> Analyzer ()
{-# INLINE pushGlobal #-}
pushGlobal sym = modifyTable_ . insertGlobal sym

pushScoped :: Symbol -> SymbolData -> Analyzer ()
{-# INLINE pushScoped #-}
pushScoped sym = modifyTable_ . insertScoped sym
