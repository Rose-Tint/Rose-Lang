{-# LANGUAGE FlexibleInstances #-}

module Data.Table (
    Data(..),
    Func(..),
    Trait(..),
    Table(..),

    insertType,
    insertTrait,
    insertGlobal,
    insertScoped,

    lookupType,
    lookupTrait,
    lookupGlobal,
    lookupScoped,
    lookupScoped',
    lookupNewestScope,

    mkCtor,

    addScope,
    remScope,
    emptyTable,
    unionTable,

    getSimilarVars,
) where

import Data.Binary
import Data.Maybe (mapMaybe)

import Common.SrcPos
import Common.Specifiers
import Common.Var
import Data.VarMap as M
import Text.Pretty
import Typing.Primitives
import Typing.Type
import Utils.String


default (Int, Double)


data Data = Data {
    dataType :: Type,
    dataCtors :: [Var],
    dataPos :: SrcPos
    }

data Func = Func {
    funcType :: Type,
    funcPur :: Purity,
    funcPos :: SrcPos
    }

data Trait = Trait {
    traitMeths :: [Var],
    traitImpls :: [Type],
    traitPos :: SrcPos
    }

type DataMap = VarMap Data

type TraitMap = VarMap Trait

type GlobalMap = VarMap Func

type ScopedMaps = [VarMap Func]

data Table = Table {
    tblTypes :: DataMap,
    tblTraits :: TraitMap,
    tblGlobals :: GlobalMap,
    tblScopeds :: ScopedMaps
    }


unionTable :: Table -> Table -> Table
unionTable (Table tys1 trs1 gls1 scs1) (Table tys2 trs2 gls2 scs2) =
    Table (tys1 <> tys2) (trs1 <> trs2) (gls1 <> gls2)
    (zipWith (<>) scs1 scs2)

emptyTable :: Table
emptyTable = Table
    (fromList [
        (prim "Bool", primData
            boolType
            [prim "True", prim "False"]
            ),
        (prim "Int", primData intType []),
        (prim "Float", primData floatType []),
        (prim "Double", primData doubleType []),
        (prim "String", primData stringType []),
        (prim "Char", primData charType []),
        (prim "[]", primData (arrayOf tv) []),
        (prim "(,)", primData (tupleOf []) [])
    ])
    empty
    (fromList [
        (prim "True", primFunc boolType),
        (prim "False", primFunc boolType)
    ])
    []
    where
        tv = TypeVar (prim "")
        primFunc t = Func t Pure UnknownPos
        primData t cs = Data t cs UnknownPos

insertType :: Var -> Data -> Table -> Table
insertType sym dta tbl = tbl {
    tblTypes = insert sym dta (tblTypes tbl)
    }

insertTrait :: Var -> Trait -> Table -> Table
insertTrait sym dta tbl = tbl {
    tblTraits = insert sym dta (tblTraits tbl)
    }

insertGlobal :: Var -> Func -> Table -> Table
insertGlobal sym dta tbl = tbl {
    tblGlobals = insert sym dta (tblGlobals tbl)
    }

insertScoped :: Var -> Func -> Table -> Table
insertScoped sym dta tbl =
    let insert' = insert sym dta
    in case tblScopeds tbl of
        [] -> tbl { tblScopeds = [insert' empty] }
        (scp:scps) -> tbl { tblScopeds = (insert' scp:scps) }

mkCtor :: Var -> Type -> Func
mkCtor name scheme = Func scheme Pure (getPos name)

lookupType :: Var -> Table -> Maybe Data
lookupType name = M.lookup name . tblTypes

lookupTrait :: Var -> Table -> Maybe Trait
lookupTrait name = M.lookup name . tblTraits

lookupGlobal :: Var -> Table -> Maybe Func
lookupGlobal name = M.lookup name . tblGlobals

lookupScoped :: Var -> Table -> Maybe Func
lookupScoped name tbl = case lookupScoped' name tbl of
    Nothing -> lookupGlobal name tbl
    Just dta -> Just dta

lookupScoped' :: Var -> Table -> Maybe Func
lookupScoped' name = foldl (\prev scp -> case prev of
    Nothing -> M.lookup name scp
    Just dta -> Just dta
    ) Nothing . tblScopeds

lookupNewestScope :: Var -> Table -> Maybe Func
lookupNewestScope name tbl = case tblScopeds tbl of
    [] -> lookupGlobal name tbl
    scps -> M.lookup name (last scps)

addScope :: Table -> Table
addScope tbl = tbl { tblScopeds = (empty:tblScopeds tbl) }

remScope :: Table -> Table
remScope tbl = tbl {
    tblScopeds = case tblScopeds tbl of
        [] -> []
        (_:scps) -> scps
    }

getSimilarVars :: Var -> Table -> [Var]
getSimilarVars (Var name _) tbl =
    concat [
        filtKeys (tblTypes tbl),
        filtKeys (tblTraits tbl),
        filtKeys (tblGlobals tbl),
        concatMap filtKeys (tblScopeds tbl)
    ]
    where
        filtKeys :: HasSrcPos a => VarMap a -> [Var]
        filtKeys = mapMaybe (\(key, scp) ->
            if areSimilar name key then
                Just (Var key (getPos scp))
            else
                Nothing
            ) . assocs


instance HasSrcPos Data where
    getPos = dataPos

instance HasSrcPos Func where
    getPos = funcPos

instance HasSrcPos Trait where
    getPos = traitPos

instance Pretty (String, Data) where
    pretty (name, Data typ _ctors _pos) = 10.<name|+" | "+|30.<typ

instance Pretty (String, Func) where
    pretty (name, Func typ _pur _pos) = 10.<name|+" | "+|30.<typ

instance Pretty Table where
    pretty (Table typs _trts glbs scps) =
        "Types:\n"+|indentCatLns (M.assocs typs)|+"\n\n"+|
        "Globals:\n"+|indentCatLns (M.assocs glbs)|+"\n\n"+|
        "Scopeds:\n"+|indentCatLns (concatMap M.assocs scps)

instance Binary Data where
    put (Data typ ctors pos) = do
        put typ
        putList ctors
        put pos
    get = Data <$> get <*> get <*> get

instance Binary Func where
    put (Func typ pur pos) = do
        put typ
        put pur
        put pos
    get = Func <$> get <*> get <*> get

instance Binary Trait where
    put (Trait meths impls pos) = do
        putList meths
        putList impls
        put pos
    get = Trait <$> get <*> get <*> get

instance Binary Table where
    put (Table types traits glbs _scps) = do
        put types
        put traits
        put glbs
    get = Table <$> get <*> get <*> get <*> pure []
