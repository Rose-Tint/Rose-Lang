module AST.Value (
    Value(..),
    ValArray,
    valueFromList
) where

import Data.Array
import Data.Char (isAlphaNum)

import AST.Literal
import AST.Pattern
import Common.SrcPos
import Common.Var
import Text.Pretty


type ValArray = Array Int Value

data Value
    = Literal Literal
    | VarVal Var
    | Application Value Value
    | CtorCall Var
    | Tuple ValArray
    | Array ValArray
    | Lambda [Var] Value
    | IfElseVal Value Value Value
    | MatchVal Value [(Pattern, Value)]


valueFromList :: Value -> [Value] -> Value
valueFromList val [] = val
valueFromList v1 (v2:vs) = Application v1 (valueFromList v2 vs)


instance HasSrcPos Value where
    getPos (Literal lit) = getPos lit
    getPos (VarVal var) = getPos var
    getPos (Application val _) = getPos val
    getPos (CtorCall name) = getPos name
    getPos (Lambda [] val) = getPos val
    getPos (Lambda (val:_) _) = getPos val
    getPos (IfElseVal val _ _) = getPos val
    getPos (MatchVal val _) = getPos val
    getPos _ = UnknownPos

instance Pretty Value where
    terse (Literal l) = terse l
    terse (VarVal var) = terse var
    terse (Tuple arr) = "("-|","`sepsT`elems arr|-")"
    terse (Array arr) = "["-|","`sepsT`elems arr|-"]"
    terse val = pretty val
    pretty (Literal l) = pretty l
    pretty (VarVal (Var name _))
        | all isIdChar name = name
        | otherwise = "("+|name|+")"
        where
            isIdChar c = isAlphaNum c || c == '_'
    pretty (Application val arg) =
        "("+|val|+" "+|arg|+")"
    pretty (CtorCall name) = "("+|name|+")"
    pretty (Tuple arr) = "("+|", "`seps`elems arr|+")"
    pretty (Array arr) = "[ "+|", "`seps`elems arr|+" ]"
    pretty (Lambda ps body) =
        "("+|" "`seps`ps|+" => "+|body|+")"
    pretty (IfElseVal cnd tr fa) =
        "(if ("+|cnd|+") then "+|tr|+" else "+|fa|+")"
    pretty MatchVal{} = "{- MATCH-VALUE (im lazy...) -}"
