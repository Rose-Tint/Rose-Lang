{-# LANGUAGE FlexibleInstances #-}

module SymbolTable.SymbolData (
    -- module SymbolTable.Attrs,
    Symbol,
    SymbolData(..),
    stitchSD,
    mkSymbolData,
    undef,
    -- isWellDefined,
    -- isUndefined,
    ifDefined,
    -- addAttr, addPragma, hasAttr,
) where

import Common.SrcPos
import Common.Typing.Type
import Common.Var
import Parser.Data
-- import SymbolTable.Attrs hiding (addPragma)
-- import qualified SymbolTable.Attrs as A


default (Int, Double)


type Symbol = Var

data SymbolData
    = SymbolData {
        sdType :: !Type,
        sdVisib :: Maybe Visibility,
        sdPurity :: Maybe Purity,
        sdPos :: Maybe SrcPos
        -- sdAttrs :: Attrs
    }
    deriving (Eq)


stitchSD :: SymbolData -> SymbolData -> SymbolData
{-# INLINE stitchSD #-}
stitchSD sd1 sd2 = sd1 { sdType = sdType sd1 <:> sdType sd2 }

mkSymbolData :: Symbol -> Type -> Maybe Visibility
          -> Maybe Purity -> SymbolData
{-# INLINE mkSymbolData #-}
mkSymbolData sym typ vis pur = SymbolData
    typ vis pur (Just (varPos sym))-- emptyAttrs

-- addAttr :: Attrs -> SymbolData -> SymbolData
-- {-# INLINE addAttr #-}
-- addAttr as sd = sd { sdAttrs = mergeAttrs (sdAttrs sd) as }

-- addPragma :: Pragma -> SymbolData -> SymbolData
-- {-# INLINE addPragma #-}
-- addPragma pr sd = sd { sdAttrs = A.addPragma pr (sdAttrs sd) }

-- hasAttr :: SymbolData -> Attrs -> Bool
-- {-# INLINE hasAttr #-}
-- hasAttr = testAttrs . sdAttrs

undef :: SymbolData
{-# INLINE undef #-}
undef = SymbolData {
    sdType = Delayed,
    sdVisib = Nothing,
    sdPurity = Nothing,
    sdPos = Nothing
    -- sdAttrs = emptyAttrs
}

-- isWellDefined :: SymbolData-> Bool
-- {-# INLINABLE isWellDefined #-}
-- isWellDefined dta = isJust (sdVisib dta)
--              && isJust (sdPurity dta)
--              && isComplete (sdType dta)
--              && (isJust (sdPos dta)
--                 && posModule (fromJust $! sdPos dta)
--                      /= UnknownMod)

-- isUndefined :: SymbolData-> Bool
-- {-# INLINABLE isUndefined #-}
-- isUndefined dta = isNothing (sdVisib dta)
--                && isNothing (sdPurity dta)
--                && sdType dta == NoType
--                && (isNothing (sdPos dta)
--                 || posModule (fromJust $! sdPos dta)
--                      == UnknownMod)

-- TEMPORARY
ifDefined :: SymbolData -> Maybe SymbolData
ifDefined = Just
-- {-# INLINE ifDefined #-}
-- ifDefined dta = if isWellDefined dta then Just dta else Nothing


-- instance Pretty (String, SymbolData) where
--     pretty (sym, SymbolData typ vis pur _ _) = printf
--         "| %13s | %30s | %6s | %6s |"
--         (pretty sym)
--         (pretty typ)
--         (maybe' vis)
--         (maybe' pur)
--         where
--             maybe' :: (Pretty a) => Maybe a -> String
--             maybe' a = maybe "" pretty a
--     detailed (sym, SymbolData typ vis pur _ _) = printf
--         "| %18s | %40s | %6s | %6s |"
--         (detailed sym)
--         (detailed typ)
--         (maybe' vis)
--         (maybe' pur)
--         where
--             maybe' :: (Pretty a) => Maybe a -> String
--             maybe' a = maybe "" detailed a
