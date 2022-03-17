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
insertScoped sym dta tbl = case tblScopeds tbl of
    [] -> tbl { tblScopeds = [insert sym dta empty] }
    (scp:scps) -> tbl { tblScopeds = (insert sym dta scp:scps) }


insertUndefType :: Symbol -> SymbolTable -> SymbolTable
{-# INLINABLE insertUndefType #-}
insertUndefType sym = insertType sym (undef sym)


insertUndefTrait :: Symbol -> SymbolTable -> SymbolTable
{-# INLINABLE insertUndefTrait #-}
insertUndefTrait sym = insertTrait sym (undef sym)


insertUndefGlobal :: Symbol -> SymbolTable -> SymbolTable
{-# INLINABLE insertUndefGlobal #-}
insertUndefGlobal sym = insertGlobal sym (undef sym)
