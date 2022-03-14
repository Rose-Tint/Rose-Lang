module SymbolTable.SymbolData where

import Data.Maybe

import Parser.Data


type Symbol = Variable


data SymType
    = Type [Constraint] Type
    | Delayed [Constraint]
    | TypeUndefined
    deriving (Show, Eq)


data Module = Module !String | ModUnknown
    deriving (Show, Eq, Ord)


data SymbolData
    = SymbolData {
        sdType :: SymType,
        sdVisib :: Maybe Visibility,
        sdPurity :: Maybe Purity,
        sdVar :: Variable,
        sdModule :: !Module
    }
    deriving (Show, Eq)



stitchSD :: SymbolData -> SymbolData -> SymbolData
stitchSD s1 _ = s1 -- TEMPORARY


undef :: Symbol -> SymbolData
undef sym = SymbolData {
    sdType = TypeUndefined,
    sdVisib = Nothing,
    sdPurity = Nothing,
    sdVar = sym,
    sdModule = ModUnknown
}



isDefined :: SymbolData-> Bool
isDefined dta = isJust (sdVisib dta)
             && isJust (sdPurity dta)
             && sdType dta /= TypeUndefined
             && sdModule dta /= ModUnknown


isPartDefined :: SymbolData-> Bool
isPartDefined = not . isUndefined


isUndefined :: SymbolData-> Bool
isUndefined dta = isNothing (sdVisib dta)
               && isNothing (sdPurity dta)
               && sdType dta == TypeUndefined
               && sdModule dta == ModUnknown


isPartUndefined :: SymbolData-> Bool
isPartUndefined = not . isDefined


ifDefined :: SymbolData -> Maybe SymbolData
ifDefined dta = if isDefined dta then Just dta else Nothing
