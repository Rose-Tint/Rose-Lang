module Semantics.SymbolTable where

import Data.Char (isLower, isUpper)
import Data.Map.Strict
    (Map, insert, singleton, empty)
import qualified Data.Map.Strict as Map
    (lookup, filter, toList, singleton)

import Color
import Parser.Data
import Parser.Pretty (prettyType)
import Semantics.Error
import Utils (indentAllUsing)



data SymbolLookup
    = Found SymbolData
    | NotFound !Error
    deriving (Eq)


-- For scoped variables, purity refers to
-- mutability
data SymbolData
    = Symbol {
        sdPurity :: Purity,
        sdVisib  :: Visibility,
        sdType   :: !Type,
        sdVar    :: Variable,
        sdModule :: String
    }
    deriving (Show, Eq)


type SymMap = Map Variable SymbolData


type ScopeStack = [SymMap]


-- ignore traits FOR NOW
data SymbolTable
    = SymbolTable {
        stTypes   :: !SymMap,
        -- stTraits   :: !SymMap,
        stGlobals :: !SymMap,
        stScopeds :: !ScopeStack,
        udSymbols :: !SymMap,
        udTypes   :: !SymMap
    }



primTypeData :: String -> SymbolData
primTypeData name = Symbol {
        sdPurity = Pure,
        sdVisib = Export,
        sdType = TerminalType (RealType var) [],
        sdVar = var,
        sdModule = ""
    }
    where
        var = Var name (-1) (-1) (-1)


newTable :: SymbolTable
newTable = SymbolTable
    (Map.singleton (Var "Boolean" (-1) (-1) (-1)) boolData)
    empty [] empty empty


checkScopeds :: ScopeStack -> Variable -> SymbolLookup
checkScopeds [] sym = NotFound (OutOfScope sym)
checkScopeds (curr:outters) sym =
    case Map.lookup sym curr of
        Nothing  -> checkScopeds outters sym
        Just dta -> Found dta


filterInternals :: SymMap -> SymMap
filterInternals = Map.filter filt
    where
        filt dta = case sdVisib dta of
            Intern -> False
            Export -> True


-- helpfull for ctors because it skips checking
-- scopes, and ctors cannot be scoped
lookupGlobal :: SymbolTable -> Variable -> SymbolLookup
lookupGlobal st sym = case Map.lookup sym (stGlobals st) of
    Nothing -> case Map.lookup sym (udTypes st) of
        Nothing -> NotFound (OutOfScope sym)
        Just dta -> Found dta
    Just dta -> Found dta

-- used for functions (and variables, which are 
-- technically functions internally)
lookupSymbol :: SymbolTable -> Variable -> SymbolLookup
lookupSymbol st sym =
    if (isUpper (head (varName sym))) then -- checks if ctor
        globalCheck
    else
        case checkScopeds (stScopeds st) sym of
            Found typ  -> Found typ
            NotFound _ -> globalCheck
    where
        globalCheck = lookupGlobal st sym

lookupTypename :: SymbolTable -> Variable -> SymbolLookup
lookupTypename st name =
    if isLower (head (varName name)) then
        NotFound (OtherError "malformed typename")
    else
        case Map.lookup name (stTypes st) of
            Nothing -> case Map.lookup name (udTypes st) of
                Nothing -> NotFound (OutOfScope name)
                Just dta -> Found dta
            Just dta -> Found dta


addUndefType :: Variable -> SymbolData -> SymbolTable -> SymbolTable
addUndefType sym dta st =
    st { udTypes = insert sym dta (udTypes st) }

addUndefSym :: Variable -> SymbolData -> SymbolTable -> SymbolTable
addUndefSym sym dta st =
    st { udSymbols = insert sym dta (udSymbols st) }

addScope :: SymbolTable -> SymbolTable
addScope st = st { stScopeds = (empty:stScopeds st) }

addScoped :: Variable -> SymbolData -> SymbolTable -> SymbolTable
addScoped sym dta st =
    let dta' = dta { sdType = simplifyType (sdType dta) }
    in case stScopeds st of
        [] -> st { stScopeds = [singleton sym dta'] }
        (scp:scps) ->
            st { stScopeds = (insert sym dta' scp:scps) }

addGlobal :: Variable -> SymbolData -> SymbolTable -> SymbolTable
addGlobal sym dta st =
    let dta' = dta { sdType = simplifyType (sdType dta) }
    in st { stGlobals = insert sym dta' (stGlobals st) }

addType :: Variable -> SymbolData -> SymbolTable -> SymbolTable
addType name dta st =
    let dta' = dta { sdType = simplifyType (sdType dta) }
    in st { stTypes = insert name dta' (stTypes st) }


removeScope :: SymbolTable -> SymbolTable
removeScope st = let scps = stScopeds st in
    case scps of
        [] -> st
        (_:scps') -> st { stScopeds = scps' }


boolData :: SymbolData
boolData = primTypeData "Boolean"


prettySymbolData :: (Variable, SymbolData) -> String
prettySymbolData (sym, dta) = printf
    "%15s | %30s"
    (varName sym) (prettyType (sdType dta))


prettySymMap :: SymMap -> String
prettySymMap m = indentAllUsing prettySymbolData (Map.toList m)


prettySymbolTable :: SymbolTable -> String
prettySymbolTable st = printf
    "       | Symbol Name    | Symbol Type\n\
    \Types:\n%s\
    \Globals:\n%s\
    \Scopeds:\n%s\
    \Undefined Types:\n%s\
    \Undefined Symbols:\n%s"
    (prettySymMap (stTypes st))
    -- (prettySymMap (stTraits st))
    (prettySymMap (stGlobals st))
    (concat (fmap prettySymMap (stScopeds st)))
    (prettySymMap (udTypes st))
    (prettySymMap (udSymbols st))
