module SymbolTable.SymbolData where

import Control.Applicative (Alternative((<|>)))
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
        sdVisib :: Maybe Visibility,
        sdPurity :: Maybe Purity,
        sdVar :: !Variable,
        sdModule :: !Module
    }
    deriving (Show, Eq)



stitchSD :: SymbolData -> SymbolData -> SymbolData
stitchSD sd1 sd2 = sd1 {
        sdType = sdType sd1 <~> sdType sd2,
        sdVisib =
            let sv1 = sdVisib sd1
                sv2 = sdVisib sd2
            in if isJust sv1 && isJust sv2 then
                if sv1 == (Just Intern) then sv1 else sv2
            else
                sv1 <|> sv2,
        sdPurity =
            let sv1 = sdPurity sd1
                sv2 = sdPurity sd2
            in if isJust sv1 && isJust sv2 then
                if sv1 /= (Just Pure) then sv1 else sv2
            else
                sv1 <|> sv2
    }



undef :: Symbol -> SymbolData
{-# INLINE undef #-}
undef sym = SymbolData {
    sdType = NoType,
    sdVisib = Nothing,
    sdPurity = Nothing,
    sdVar = sym,
    sdModule = ModUnknown
}



isWellDefined :: SymbolData-> Bool
{-# INLINABLE isWellDefined #-}
isWellDefined dta = isJust (sdVisib dta)
             && isJust (sdPurity dta)
             && isComplete (sdType dta)
             && sdModule dta /= ModUnknown


isUndefined :: SymbolData-> Bool
{-# INLINABLE isUndefined #-}
isUndefined dta = isNothing (sdVisib dta)
               && isNothing (sdPurity dta)
               && sdType dta == NoType
               && sdModule dta == ModUnknown


ifDefined :: SymbolData -> Maybe SymbolData
{-# INLINE ifDefined #-}
ifDefined dta = if isWellDefined dta then Just dta else Nothing
