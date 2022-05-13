module Data.Table (
    module T,
    Table(..),
    ScopedMaps,
    insertType,
    insertTrait,
    insertGlobal,
    insertScoped,
    emptyTable,
    getSimilarVars
) where

import Data.Maybe (mapMaybe)

import Common.SrcPos
import Common.Var
import Data.Table.Datatype as T
import Data.Table.Global as T
import Data.Table.Scoped as T
import Data.Table.Trait as T
import Data.VarMap as T
import Text.Pretty
import Utils.String


default (Int, Double)


type DataMap = Trie Datatype

type TraitMap = Trie Trait

type GlobalMap = Trie Global

type ScopedMaps = [Trie Scoped]

data Table
    = Table {
        tblTypes :: !DataMap,
        tblTraits :: !TraitMap,
        tblGlobals :: !GlobalMap,
        tblScopeds :: !ScopedMaps
    }
    -- deriving (Eq)


emptyTable :: Table
emptyTable = Table empty empty empty []

insertType :: Var -> Datatype -> Table -> Table
insertType sym dta tbl = tbl {
    tblTypes = insert sym dta (tblTypes tbl)
    }

insertTrait :: Var -> Trait -> Table -> Table
insertTrait sym dta tbl = tbl {
    tblTraits = insert sym dta (tblTraits tbl)
    }

insertGlobal :: Var -> Global -> Table -> Table
insertGlobal sym dta tbl = tbl {
    tblGlobals = insert sym dta (tblGlobals tbl)
    }

insertScoped :: Var -> Scoped -> Table -> Table
insertScoped sym dta tbl =
    let insert' = insert sym dta
    in case tblScopeds tbl of
        [] -> tbl { tblScopeds = [insert' empty] }
        (scp:scps) -> tbl { tblScopeds = (insert' scp:scps) }

getSimilarVars :: Var -> Table -> [Var]
getSimilarVars (Var name _) (Table typs trts glbs scps) =
    concat [
        filtKeys dtPos typs,
        filtKeys trtPos trts,
        filtKeys glbPos glbs,
        concatMap (filtKeys scpPos) scps
    ]
    where
        filtKeys :: (a -> SrcPos) -> Trie a -> [Var]
        filtKeys f = mapMaybe (\(key, scp) ->
            if areSimilar name key then
                Just (Var key (f scp))
            else
                Nothing
            ) . assocs


instance Pretty Table where
    pretty (Table types trts glbs scps) =
        types|+"\n\n"+|trts|+"\n\n"+|glbs|+"\n\n"+|scps
