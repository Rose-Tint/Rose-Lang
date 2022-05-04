{-# LANGUAGE FlexibleInstances #-}

module Middle.SymbolTable.SymbolData (
    -- module SymbolTable.Attrs,
    Symbol,
    SymbolData(..),
    stitchSD,
    mkSymbolData,
    undef,
    ifDefined,
) where

import Common.SrcPos
import Common.Typing.Type
import Common.Var
import Front.Parser
import Pretty


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
    typ vis pur (Just (varPos sym))

undef :: SymbolData
{-# INLINE undef #-}
undef = SymbolData {
    sdType = Delayed,
    sdVisib = Nothing,
    sdPurity = Nothing,
    sdPos = Nothing
}

-- TEMPORARY
ifDefined :: SymbolData -> Maybe SymbolData
ifDefined = Just
-- {-# INLINE ifDefined #-}
-- ifDefined dta = if isWellDefined dta then Just dta else Nothing


-- Key-Value pair
instance Pretty (String, SymbolData) where
    terse (sym, SymbolData typ _ _ _) =
        "| "-|10.<sym|-" | "-|25.<typ|-" |"
    pretty (sym, SymbolData typ vis _ _) =
        "| "+|15.<sym|+" | "+|30.<typ|+" | "+|6.<vis|+" |"
    detailed (sym, SymbolData typ vis pur pos) =
         "| "*|20.<sym|*" | "*|10.<pos|*" | "*|35.<typ|*
        " | "*|6.<vis|*" | "*|6.<pur|*" |"
