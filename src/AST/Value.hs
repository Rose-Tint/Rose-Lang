module AST.Value (
    Value(..),
    ValArray,
    valPos,
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
    | Application Value [Value]
    | CtorCall Var [Value]
    | Tuple ValArray
    | Array ValArray
    | Lambda [Var] Value
    | IfElseVal Value Value Value
    | MatchVal Value [(Pattern, Value)]


valPos :: Value -> SrcPos
valPos (Literal (IntLit _ p)) = p
valPos (Literal (FloatLit _ p)) = p
valPos (Literal (DoubleLit _ p)) = p
valPos (Literal (CharLit _ p)) = p
valPos (Literal (StringLit _ p)) = p
valPos (VarVal var) = varPos var
valPos (CtorCall name _) = varPos name
valPos _ = UnknownPos


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
    pretty (Application val args) =
        "("+|val|+" "+|" "`seps`args|+")"
    pretty (CtorCall name args) =
        "("+|name|+" "+|" "`seps`args|+")"
    pretty (Tuple arr) = "("+|", "`seps`elems arr|+")"
    pretty (Array arr) = "[ "+|", "`seps`elems arr|+" ]"
    pretty (Lambda ps body) =
        "("+|" "`seps`ps|+" => "+|body|+")"
    pretty (IfElseVal cnd tr fa) =
        "(if ("+|cnd|+") then "+|tr|+" else "+|fa|+")"
    pretty MatchVal{} = "{- MATCH-VALUE (im lazy...) -}"
