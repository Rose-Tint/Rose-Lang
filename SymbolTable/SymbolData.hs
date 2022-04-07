{-# LANGUAGE FlexibleInstances #-}

module SymbolTable.SymbolData where

-- import Control.Applicative (Alternative((<|>)))
import Data.Maybe

import Color
import Parser.Data hiding (Type)
import Pretty
import Typing.Types


default (Int, Double)


type Symbol = Variable


data SymbolData
    = SymbolData {
        sdType :: !Type,
        sdVisib :: Maybe Visibility,
        sdPurity :: Maybe Purity,
        sdPos :: Maybe Position
    }
    deriving (Show, Eq)



stitchSD :: SymbolData -> SymbolData -> SymbolData
{-# INLINE stitchSD #-}
stitchSD sd1 sd2 = sd1 { sdType = sdType sd1 <~> sdType sd2 }


mkSymbolData :: Symbol -> Type -> Maybe Visibility
          -> Maybe Purity -> SymbolData
{-# INLINE mkSymbolData #-}
mkSymbolData sym typ vis pur = SymbolData
    typ vis pur (Just (varPos sym))


undef :: SymbolData
{-# INLINE undef #-}
undef = SymbolData {
    sdType = Delayed [],
    sdVisib = Nothing,
    sdPurity = Nothing,
    sdPos = Nothing
}



isWellDefined :: SymbolData-> Bool
{-# INLINABLE isWellDefined #-}
isWellDefined dta = isJust (sdVisib dta)
             && isJust (sdPurity dta)
             && isComplete (sdType dta)
             && (isJust (sdPos dta)
                && posModule (fromJust $! sdPos dta)
                     /= UnknownMod)


isUndefined :: SymbolData-> Bool
{-# INLINABLE isUndefined #-}
isUndefined dta = isNothing (sdVisib dta)
               && isNothing (sdPurity dta)
               && sdType dta == NoType
               && (isNothing (sdPos dta)
                || posModule (fromJust $! sdPos dta)
                     == UnknownMod)


ifDefined :: SymbolData -> Maybe SymbolData
{-# INLINE ifDefined #-}
ifDefined dta = if isWellDefined dta then Just dta else Nothing



instance Pretty (String, SymbolData) where
    pretty (sym, SymbolData typ vis pur _) = printf
        "| %13s | %30s | %6s | %6s |"
        (pretty sym)
        (pretty typ)
        (maybe' vis)
        (maybe' pur)
        where
            maybe' :: (Pretty a) => Maybe a -> String
            maybe' a = maybe "" pretty a
    detailed (sym, SymbolData typ vis pur _) = printf
        "| %18s | %40s | %6s | %6s |"
        (detailed sym)
        (detailed typ)
        (maybe' vis)
        (maybe' pur)
        where
            maybe' :: (Pretty a) => Maybe a -> String
            maybe' a = maybe "" detailed a
