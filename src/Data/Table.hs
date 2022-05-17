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

    mkCtor,

    addScope,
    remScope,
    emptyTable,

    getSimilarVars,
) where

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
    dataVis :: Visib,
    dataCtors :: [Var],
    dataPos :: SrcPos
    }

data Func = Func {
    funcType :: Type,
    funcVis :: Visib,
    funcPur :: Purity,
    funcMut :: Mutab,
    funcPos :: SrcPos
    }

data Trait = Trait {
    traitVis :: Visib,
    traitMeths :: [Var],
    traitImpls :: [Type],
    traitPos :: SrcPos
    }

type DataMap = Trie Data

type TraitMap = Trie Trait

type GlobalMap = Trie Func

type ScopedMaps = [Trie Func]

data Table = Table {
    tblTypes :: DataMap,
    tblTraits :: TraitMap,
    tblGlobals :: GlobalMap,
    tblScopeds :: ScopedMaps
    }


emptyTable :: Table
emptyTable = Table
    (M.fromList [
        (prim "Bool", primData
            boolType
            [prim "True", prim "False"]
            ),
        (prim "Int", primData intType []),
        (prim "Float", primData floatType []),
        (prim "Double", primData doubleType []),
        (prim "String", primData stringType []),
        (prim "Char", primData charType []),
        (prim "[]", primData
            (arrayOf (TypeVar (prim "a")))
            []),
        (prim "(,)", primData
            (arrayOf (TypeVar (prim "a")))
            [])
    ]) empty empty []
    where
        primData t cs = Data t Export cs UnknownPos

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

mkCtor :: Var -> Visib -> Type -> Func
mkCtor name vis scheme = Func scheme vis Pure Imut (getPos name)

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
        filtKeys :: HasSrcPos a => Trie a -> [Var]
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

instance Pretty Table where
    pretty = const ""
    -- pretty (Table types trts glbs scps) =
    --     types|+"\n\n"+|trts|+"\n\n"+|glbs|+"\n\n"+|scps
