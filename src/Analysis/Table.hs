module Analysis.Table (
    module Data.Table,
    -- searchDatatypes, searchTraits, searchGlobals, searchScopeds,
    findDatatype,
    findTrait,
    findGlobal,
    findScoped,

    lookupType,
    lookupTrait,
    lookupGlobal,
    lookupScoped,

    modifyDatatype,
    modifyTrait,

    pushDatatype,
    pushTrait,
    pushUndefTrait,
    pushCtor,
    pushUndefCtor,
    pushMethod,
    pushFunction,
    pushUndefFunc,
    pushFunction',
    pushScoped,
) where

import Prelude hiding (lookup)

import Analysis.Analyzer
import Common.Specifiers
import Common.SrcPos
import Common.Var
import Data.Table


-- Thus is the convention for Var Table lookups:
--   - `search*` always returns a `SymbolData` object.
--       It will insert an "undefined" symbol into the
--       appropriate table if it is not found.
--   - `lookup*` returns a `Maybe SymbolData` object.
--       `Nothing` if not found, and `Just dta` if found.
--   - `find*` will throw an error if it was not found.


-- impossible because it requires a `Kind`, and we cannot
-- get that from just a `Var`
-- searchDatatypes :: Var -> Analyzer Data
-- searchDatatypes name = do
--     typs <- gets (tblTypes . table)
--     case lookup name typs of
--         Nothing -> do
--             let dta = undefined
--             modifyTable (insertType name dta)
--             return dta
--         Just dta -> return $! dta

-- impossible because it requires a `Kind`, and we cannot
-- get that from just a `Var`
-- searchTraits :: Var -> Analyzer Trait
-- searchTraits name = do
--     trts <- gets (tblTraits . table)
--     case lookup name trts of
--         Nothing -> do
--             let dta = undefined
--             modifyTable (insertTrait name dta)
--             return dta
--         Just dta -> return $! dta

-- impossible because we dont know what type of
-- global it is
-- searchGlobals :: Var -> Analyzer Func
-- searchGlobals name = do
--     glbs <- gets (tblGlobals . table)
--     case lookup name glbs of
--         Nothing -> do
--             eT <- peekExpType
--             let dta = mkSymbolData name eT Nothing Nothing
--             expType <- peekExpType
--             dta' <- case expType of
--                 NoType -> return dta
--                 typ -> return $! dta
--                     { sdType = typ <::> sdType dta }
--             modifyTable (insertGlobal name dta')
--             return dta
--         Just dta -> return $! dta

-- just no.
-- searchScopeds :: Var -> Analyzer Func
-- searchScopeds name = do
--     scps <- gets (tblScopeds . table)
--     go scps
--     where
--         go [] = searchGlobals name
--         go (scp:scps) = maybe (go scps)
--             return (lookup name scp)

findDatatype :: Var -> Analyzer Data
findDatatype name = do
    mData <- lookupType name
    case mData of
        Nothing -> do
            throwUndefined name
            return undefined -- TODO:!!!
        Just dta -> return dta

findTrait :: Var -> Analyzer Trait
findTrait name = do
    mData <- lookupTrait name
    case mData of
        Nothing -> do
            throwUndefined name
            return undefined -- TODO:!!!
        Just dta -> return dta

findGlobal :: Var -> Analyzer Func
findGlobal name = do
    mData <- lookupGlobal name
    case mData of
        Nothing -> do
            throwUndefined name
            return undefined -- TODO:!!!
        Just dta -> return dta

findScoped :: Var -> Analyzer Func
findScoped name = do
    mData <- lookupScoped name
    case mData of
        Nothing -> do
            throwUndefined name
            return undefined -- TODO:!!!
        Just dta -> return dta


lookupType :: Var -> Analyzer (Maybe Data)
lookupType name = do
    types <- gets (tblTypes . table)
    return $! lookup name types

lookupTrait :: Var -> Analyzer (Maybe Trait)
lookupTrait name = do
    trts <- gets (tblTraits . table)
    return $! lookup name trts

lookupGlobal :: Var -> Analyzer (Maybe Func)
lookupGlobal name = do
    glbs <- gets (tblGlobals . table)
    return $! lookup name glbs

lookupScoped :: Var -> Analyzer (Maybe Func)
lookupScoped name = do
    scps <- gets (tblScopeds . table)
    go scps
    where
        go :: ScopedMaps -> Analyzer (Maybe Func)
        go [] = return Nothing
        go (scp:scps) = case lookup name scp of
            Nothing -> go scps
            Just dta -> return (Just dta)

modifyDatatype :: Var -> (Data -> Data) -> Analyzer ()
modifyDatatype name f = modifyTable $ \tbl ->
    tbl { tblTypes = adjust f name (tblTypes tbl) }

modifyTrait :: Var -> (Trait -> Trait) -> Analyzer ()
modifyTrait name f = modifyTable $ \tbl ->
    tbl { tblTraits = adjust f name (tblTraits tbl) }


pushDatatype :: Var -> Visib -> Analyzer Data
pushDatatype name vis = do
    let dta = Data vis [] (getPos name)
    modifyTable (insertType name dta)
    return dta

pushTrait :: Var -> Visib -> Analyzer Trait
pushTrait name vis = do
    let dta = Trait vis [] [] (getPos name)
    modifyTable (insertTrait name dta)
    return dta

pushUndefTrait :: Var -> Analyzer Trait
pushUndefTrait name = do
    let dta = Trait Export [] [] (getPos name)
    modifyTable (insertTrait name dta)
    return dta

pushCtor :: Var -> Visib -> Var -> Analyzer Func
pushCtor name vis parent = do
    let dta = Constructor vis parent (getPos name)
    modifyTable (insertGlobal name dta)
    modifyDatatype parent (\d -> d { dataCtors = (name:dataCtors d) })
    return dta

pushUndefCtor :: Var -> Analyzer Func
pushUndefCtor name = do
    -- !!!TODO: undefined
    let dta = Constructor Export (prim "UNDEFINED") (getPos name)
    modifyTable (insertGlobal name dta)
    return dta

pushFunction :: Var -> Visib -> Analyzer Func
pushFunction name vis = do
    let dta = Function vis Nothing (getPos name)
    modifyTable (insertGlobal name dta)
    return dta

pushUndefFunc :: Var -> Analyzer Func
pushUndefFunc name = do
    -- TODO: get current/expoected purity
    -- instead of `Nothing` or `Unsafe`
    let dta = Function Export Nothing (getPos name)
    modifyTable (insertGlobal name dta)
    return dta

pushFunction' :: Var -> Visib -> Purity -> Analyzer Func
pushFunction' name vis pur = do
    let dta = Function vis (Just pur) (getPos name)
    modifyTable (insertGlobal name dta)
    return dta

pushScoped :: Var -> Mutab -> Analyzer Func
pushScoped name mut = do
    scheme <- lift (fresh >>= generalize)
    let dta = Func scheme mut (getPos name)
    lift $ modifyTable (insertScoped name dta)
    return dta
