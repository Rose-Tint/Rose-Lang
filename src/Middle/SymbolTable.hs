module Middle.SymbolTable (
    module S,
    SymbolTable(..),
    insertType,
    insertTrait,
    insertGlobal,
    insertScoped,
    emptyTable,
    getSimilarSymbols
) where

import Data.Maybe (mapMaybe)

import Common.Var
import Middle.SymbolTable.SymbolData as S
import Middle.SymbolTable.SymbolMap as S
import Middle.SymbolTable.Trie (assocs)
import Pretty
import Utils.String


default (Int, Double)


data SymbolTable
    = SymbolTable {
        tblTypes :: !SymbolMap,
        tblTraits :: !SymbolMap,
        tblGlobals :: !SymbolMap,
        tblScopeds :: ![SymbolMap]
    }
    deriving (Eq)


emptyTable :: SymbolTable
{-# INLINE emptyTable #-}
emptyTable = SymbolTable empty empty empty []

insertType :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
{-# INLINABLE insertType #-}
insertType sym dta tbl = tbl { tblTypes = insert sym dta (tblTypes tbl) }

insertTrait :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
{-# INLINABLE insertTrait #-}
insertTrait sym dta tbl = tbl { tblTraits = insert sym dta (tblTraits tbl) }

insertGlobal :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
{-# INLINABLE insertGlobal #-}
insertGlobal sym dta tbl = tbl { tblGlobals = insert sym dta (tblGlobals tbl) }

insertScoped :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
{-# INLINABLE insertScoped #-}
insertScoped sym dta tbl = let insert' = insert sym dta in
    case tblScopeds tbl of
        [] -> tbl { tblScopeds = [insert' empty] }
        (scp:scps) -> tbl { tblScopeds = (insert' scp:scps) }

getSimilarSymbols :: Symbol -> SymbolTable -> [Symbol]
{-# INLINABLE getSimilarSymbols #-}
getSimilarSymbols sym (SymbolTable typs trts glbs scps) =
    let var = varName sym
        filt = mapMaybe (\(key, dta) ->
            if areSimilar var key then
                Just $! maybe (prim key) (Var key) (sdPos dta)
            else
                Nothing) . assocs
        scpKeys = concatMap filt scps
    in concatMap filt [typs, trts, glbs] ++ scpKeys


newtype ScopedTable = ScpTbl [SymbolMap]

instance Pretty ScopedTable where
    pretty (ScpTbl scps) =
        "+-Symbol#10-+-Position--+-Type#32-+-Visib.-+-Purity-+\n"
        +|(unlines $! detailed <$> concatMap assocs scps)|+
        "+#17-+#11-+#37-+#8-+#8-+"


instance Pretty SymbolTable where
    pretty (SymbolTable typs trts glbs scps) =
            "Type Table:\n"+|typs|+
        "\n\nTrait Table:\n"+|trts|+
        "\n\nTop-Level Table:\n"+|glbs|+
        "\n\nFunction-Local Table:\n"+|ScpTbl scps|+
        "\n\n"
