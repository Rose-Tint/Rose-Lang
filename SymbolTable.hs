module SymbolTable (
    module SymbolTable.SymbolData,
    module SymbolTable.SymbolMap,
    SymbolTable(..),
    insertType,
    insertTrait,
    insertGlobal,
    insertScoped,
    emptyTable,
    getSimilarSymbols
) where

import Control.Monad ((<$!>))
import qualified Data.Map.Strict as Map (keys)

import Color
import Parser.Data (Variable(..))
import Pretty
import SymbolTable.SymbolData
import SymbolTable.SymbolMap
import Utils


default (Int, Double)



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
insertType sym dta tbl = tbl
    { tblTypes = insert sym dta (tblTypes tbl) }


insertTrait :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
{-# INLINABLE insertTrait #-}
insertTrait sym dta tbl = tbl
    { tblTraits = insert sym dta (tblTraits tbl) }


insertGlobal :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
{-# INLINABLE insertGlobal #-}
insertGlobal sym dta tbl = tbl
    { tblGlobals = insert sym dta (tblGlobals tbl) }


insertScoped :: Symbol -> SymbolData -> SymbolTable -> SymbolTable
{-# INLINABLE insertScoped #-}
insertScoped sym dta tbl = let insert' = insert sym dta in
    case tblScopeds tbl of
        [] -> tbl { tblScopeds = [insert' empty] }
        (scp:scps) -> tbl { tblScopeds = (insert' scp:scps) }


-- insertUndefType :: Symbol -> SymbolTable -> SymbolTable
-- {-# INLINABLE insertUndefType #-}
-- insertUndefType = insertType .! undef


-- insertUndefTrait :: Symbol -> SymbolTable -> SymbolTable
-- {-# INLINABLE insertUndefTrait #-}
-- insertUndefTrait = insertTrait .! undef


-- insertUndefGlobal :: Symbol -> SymbolTable -> SymbolTable
-- {-# INLINABLE insertUndefGlobal #-}
-- insertUndefGlobal = insertGlobal .! undef


getSimilarSymbols :: Symbol -> SymbolTable -> [Symbol]
getSimilarSymbols sym (SymbolTable typs trts glbs scps) =
    let filt = filter (areSimilar (varName sym) . varName) . Map.keys
    in filt typs ++ filt trts ++ filt glbs ++ (concat $! filt <$!> scps)



instance Pretty SymbolTable where
    pretty (SymbolTable typs trts glbs _) = printf
        "Type Table:\n%s\n\n\n\
        \Trait Table:\n%s\n\n\n\
        \Top-Level Table:\n%s"
        (pretty typs) (pretty trts) (pretty glbs)
