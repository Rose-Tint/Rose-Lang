module SymbolTable.SymbolTable where

import SymbolTable.SymbolData
import SymbolTable.SymbolMap


type Insertion = Symbol -> SymbolData -> SymbolTable -> SymbolTable


data SymbolTable
    = SymbolTable {
        tblTypes :: !SymbolMap,
        tblTraits :: !SymbolMap,
        tblGlobals :: !SymbolMap,
        tblScopeds :: ![SymbolMap]
    }


-- insertType :: Insertion
-- insertTrait :: Insertion
-- insertGlobal :: Insertion
-- insertScoped :: Insertion


-- searchScopeds :: Symbol -> Table
