module SymbolTable.SymbolData where

import Data.Maybe

import Parser.Data


type Symbol = String


data Module = Module String | ModUnknown
    deriving (Show, Eq, Ord)


data SymbolData
    = SymbolData {
        sdType :: Maybe Type,
        sdVisib :: Maybe Visibility,
        sdPurity :: Maybe Purity,
        sdModule :: Module
    }



stitchSD :: SymbolData -> SymbolData -> SymbolData
stitchSD s1 _ = s1 -- TEMPORARY


undefined :: SymbolData
undefined = SymbolData
    Nothing Nothing Nothing ModUnknown


isDefined :: SymbolData-> Bool
isDefined dta = isJust (sdType dta)
             && isJust (sdVisib dta)
             && isJust (sdPurity dta)
             && sdModule dta /= ModUnknown

isPartDefined :: SymbolData-> Bool
isPartDefined = not . isUndefined

isUndefined :: SymbolData-> Bool
isUndefined dta = isNothing (sdType dta)
               && isNothing (sdVisib dta)
               && isNothing (sdPurity dta)
               && sdModule dta == ModUnknown

isPartUndefined :: SymbolData-> Bool
isPartUndefined = not . isDefined


ifDefined :: SymbolData -> Maybe SymbolData
ifDefined dta = if isDefined dta then Just dta else Nothing
