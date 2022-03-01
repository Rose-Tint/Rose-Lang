module Semantics.SymbolTable where

import Data.Char (isLower, isUpper)

import Data.Map.Strict
    (Map, insert, singleton, empty)
import qualified Data.Map.Strict as Map
    (lookup)

import Parser.Data
import Semantics.Error



data SymbolLookup
    = Found SymbolData
    | NotFound Error
    deriving (Eq)


data SymbolData
    = SymData {
        sdMut :: Mutability,
        sdPurity :: Purity,
        sdVisib :: Visibility,
        sdCons :: [Constraint],
        sdType :: Type,
        sdModule :: String
    }
    deriving (Show, Eq)


type SymMap = Map String SymbolData


type ScopeStack = [SymMap]


data SymbolTable
    = SymbolTable {
        stTypes :: SymMap,
        stTraits :: SymMap, -- ignore traits FOR NOW
        stGlobals :: SymMap,
        stScopeds :: ScopeStack
    }



checkScopeds :: ScopeStack -> String -> SymbolLookup
checkScopeds [] sym = NotFound (OutOfScope sym)
checkScopeds (curr:outters) sym =
    case Map.lookup sym curr of
        Nothing  -> checkScopeds outters sym
        Just dta -> Found dta


-- helpfull for ctors because it skips checking
-- scopes, and ctors cannot be scoped
lookupGlobal :: SymbolTable -> String -> SymbolLookup
lookupGlobal st sym =
    case Map.lookup sym (stGlobals st) of
        Nothing  -> NotFound (OutOfScope sym)
        Just dta -> Found dta


-- used for functions (and variables, which are 
-- technically functions internally)
lookupSymbol :: SymbolTable -> String -> SymbolLookup
lookupSymbol st sym =
    if (isUpper (head sym)) then -- checks if ctor
        globalCheck
    else
        case checkScopeds (stScopeds st) sym of
            Found typ  -> Found typ
            NotFound _ -> globalCheck
    where
        globalCheck = lookupGlobal st sym


lookupTypename :: SymbolTable -> String -> SymbolLookup
lookupTypename st name =
    if isLower (head name) then
        NotFound (OtherError "malformed typename")
    else
        case Map.lookup name (stTypes st) of
            Nothing  -> NotFound (OutOfScope name)
            Just dta -> Found dta
    

addScope :: SymbolTable -> SymbolTable
addScope st = st {
        stScopeds = (empty:stScopeds st)
    }


addScoped, addGlobal, addType ::
    String -> SymbolData -> SymbolTable -> SymbolTable
addScoped sym dta st = case stScopeds st of
    []         -> st {
            stScopeds = [singleton sym dta]
        }
    (scp:scps) -> st {
            stScopeds = (insert sym dta scp:scps)
        }

addGlobal sym dta st = st {
        stGlobals = insert sym dta (stGlobals st)
    }

addType name dta st = st {
        stTypes = insert name dta (stTypes st)
    }


removeScope :: SymbolTable -> SymbolTable
removeScope st@(SymbolTable _ _ _ []) = st
removeScope st@(SymbolTable _ _ _ (_:scps)) = st {
        stScopeds = scps
    }


primTypeData :: String -> SymbolLookup
primTypeData name = Found $ SymData {
        sdMut = Immutable,
        sdPurity = Pure,
        sdVisib = Export,
        sdCons = [],
        sdType = TerminalType name [],
        sdModule = ""
    }


intType, fltType, chrType, strType
    :: SymbolLookup
intType = primTypeData "Int"
fltType = primTypeData "Float"
chrType = primTypeData "Char"
strType = arrType chrType
arrType :: SymbolLookup -> SymbolLookup
arrType (Found dta) = let (Found arr) = (primTypeData "") in
    Found (arr {
        sdType = TerminalType "Array" [sdType dta]
    })
arrType err = err
