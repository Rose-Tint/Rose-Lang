module SymbolTable.SymbolData where

import Data.Maybe

import Parser.Data hiding (Type)
import Typing.Types


type Symbol = Variable


data Module
    = Module Visibility !Variable
    | ModUnknown
    deriving (Show, Eq, Ord)


data SymbolData
    = SymbolData {
        sdType :: !Type,
        sdVisib :: !(Maybe Visibility),
        sdPurity :: !(Maybe Purity),
        sdVar :: !Variable,
        sdModule :: !Module
    }
    deriving (Show, Eq)



stitchSD :: SymbolData -> SymbolData -> SymbolData
stitchSD s1 _ = s1 -- TEMPORARY


undef :: Symbol -> SymbolData
undef sym = SymbolData {
    sdType = NoType,
    sdVisib = Nothing,
    sdPurity = Nothing,
    sdVar = sym,
    sdModule = ModUnknown
}



isDefined :: SymbolData-> Bool
isDefined dta = isJust (sdVisib dta)
             && isJust (sdPurity dta)
             && isComplete (sdType dta)
             && sdModule dta /= ModUnknown


isPartDefined :: SymbolData-> Bool
isPartDefined = not . isUndefined


isUndefined :: SymbolData-> Bool
isUndefined dta = isNothing (sdVisib dta)
               && isNothing (sdPurity dta)
               && sdType dta == NoType
               && sdModule dta == ModUnknown


isPartUndefined :: SymbolData-> Bool
isPartUndefined = not . isDefined


ifDefined :: SymbolData -> Maybe SymbolData
ifDefined dta = if isDefined dta then Just dta else Nothing
