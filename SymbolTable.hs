module SymbolTable (
    module SymbolTable.SymbolData,
    module SymbolTable.SymbolMap,
    SymbolTable(..),
    insertType, insertUndefType,
    insertTrait, insertUndefTrait,
    insertGlobal, insertUndefGlobal,
    insertScoped,
    emptyTable,
) where

import SymbolTable.SymbolData
import SymbolTable.SymbolMap



data SymbolTable
    = SymbolTable {
        tblTypes :: !SymbolMap,
        tblTraits :: !SymbolMap,
        tblGlobals :: !SymbolMap,
        tblScopeds :: ![SymbolMap]
    }
    deriving (Show, Eq)



emptyTable :: SymbolTable
emptyTable = SymbolTable empty empty empty []


insertType :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
insertType sym dta tbl = tbl { tblTypes = insert sym dta (tblTypes tbl) }


insertTrait :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
insertTrait sym dta tbl = tbl { tblTraits = insert sym dta (tblTraits tbl) }


insertGlobal :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
insertGlobal sym dta tbl = tbl { tblGlobals = insert sym dta (tblGlobals tbl) }


insertScoped :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
insertScoped sym dta tbl = case tblScopeds tbl of
    [] -> tbl { tblScopeds = [insert sym dta empty] }
    (scp:scps) -> tbl { tblScopeds = (insert sym dta scp:scps) }


insertUndefType :: Symbol -> SymbolTable -> SymbolTable
insertUndefType sym = insertType sym (undef sym)


insertUndefTrait :: Symbol -> SymbolTable -> SymbolTable
insertUndefTrait sym = insertTrait sym (undef sym)


insertUndefGlobal :: Symbol -> SymbolTable -> SymbolTable
insertUndefGlobal sym = insertGlobal sym (undef sym)
