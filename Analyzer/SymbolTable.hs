module Analyzer.SymbolTable where

import Prelude hiding (lookup)

import Control.Monad ((<$!>), unless)

import Analyzer.Analyzer
import Analyzer.Error
import CmdLine (CmdLine(..))
import Parser.Data (Variable(..))
import SymbolTable
import Typing.Types (Type(..))



-- Thus is the convention for Symbol Table lookups:
--   - `search*` always returns a `SymbolData` object.
--       It will insert an "undefined" symbol into the
--       appropriate table if it is not found.
--   - `find*` returns a `Maybe SymbolData` object.
--       `Nothing` if not found, and `Just dta` if found.



searchTypes :: Symbol -> Analyzer SymbolData
searchTypes sym = do
    typs <- tblTypes <$!> getTable
    case lookup sym typs of
        Nothing -> do
            let dta = undefined sym
            modifyTable (insertType sym dta)
            return dta
        Just dta -> return $! dta


searchTraits :: Symbol -> Analyzer SymbolData
searchTraits sym = do
    trts <- tblTraits <$!> getTable
    case lookup sym trts of
        Nothing -> do
            let dta = undefined sym
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
                typ -> return $! dta { sdType = typ }
            modifyTable (insertGlobal sym dta')
            return dta
        Just dta -> return $! dta


searchScopeds :: Symbol -> Analyzer SymbolData
searchScopeds sym = tblScopeds <$!> getTable >>= go >>= \res ->
    case res of
        Nothing -> do
            eT <- peekExpType
            let dta = mkSymbolData sym eT Nothing Nothing
            modifyTable (insertGlobal sym dta)
            return dta
        Just dta -> return $! dta
    where
        go [] = Just <$!> searchGlobals sym
        go (scp:scps) = case lookup sym scp of
            Just dta -> do
                allowShadowing <- fromCmdLine cmdShadowing
                unless allowShadowing $! do
                    rest <- go scps
                    case rest of
                        Nothing -> return ()
                        Just oth -> case sdPos oth of
                            Nothing -> return ()
                            Just pos -> warn $! ShadowsName sym
                                (sym { varPos = pos })
                return (Just dta)
            Nothing -> go scps


findType :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINABLE findType #-}
findType sym = do
    typs <- tblTypes <$!> getTable
    return $! lookup sym typs


findTrait :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINABLE findTrait #-}
findTrait sym = do
    trts <- tblTraits <$!> getTable
    return $! lookup sym trts


findGlobal :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINABLE findGlobal #-}
findGlobal sym = do
    glbs <- tblGlobals <$!> getTable
    return $! lookup sym glbs


findScoped :: Symbol -> Analyzer (Maybe SymbolData)
{-# INLINABLE findScoped #-}
findScoped sym = do
    scps <- tblScopeds <$!> getTable
    go scps
    where
        go :: [SymbolMap] -> Analyzer (Maybe SymbolData)
        go [] = findGlobal sym
        go (scp:scps) = case lookup sym scp of
            Nothing -> go scps
            Just dta -> return $! Just dta


modifyType :: Symbol -> (SymbolData -> SymbolData)
           -> Analyzer ()
{-# INLINABLE modifyType #-}
modifyType sym f = do
    dta <- searchTypes sym
    pushType sym $! f dta


modifyTrait :: Symbol -> (SymbolData -> SymbolData)
            -> Analyzer ()
{-# INLINABLE modifyTrait #-}
modifyTrait sym f = do
    dta <- searchTraits sym
    pushTrait sym $! f dta


modifyGlobal :: Symbol -> (SymbolData -> SymbolData)
             -> Analyzer ()
{-# INLINABLE modifyGlobal #-}
modifyGlobal sym f = do
    dta <- searchGlobals sym
    pushGlobal sym $! f dta


modifyScoped :: Symbol -> (SymbolData -> SymbolData)
             -> Analyzer ()
{-# INLINABLE modifyScoped #-}
modifyScoped sym f = do
    dta <- searchScopeds sym
    pushScoped sym $! f dta


pushType :: Symbol -> SymbolData -> Analyzer ()
{-# INLINABLE pushType #-}
pushType sym = modifyTable . insertType sym


pushTrait :: Symbol -> SymbolData -> Analyzer ()
{-# INLINABLE pushTrait #-}
pushTrait sym = modifyTable . insertTrait sym


pushGlobal :: Symbol -> SymbolData -> Analyzer ()
{-# INLINABLE pushGlobal #-}
pushGlobal sym = modifyTable . insertGlobal sym


pushScoped :: Symbol -> SymbolData -> Analyzer ()
{-# INLINABLE pushScoped #-}
pushScoped sym = modifyTable . insertScoped sym
