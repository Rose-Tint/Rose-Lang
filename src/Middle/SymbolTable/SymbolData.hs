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
    pretty = detailed
    detailed (sym, SymbolData typ vis pur pos) =
         "| "*|15.<detailed sym|*" | "+|9.<terse pos|+" | "*|35.<typ|*
        " | "*|6.<vis|*" | "*|6.<pur|*" |"