module Semantics.Analysis where

import Parser.Data
-- import Semantics.Error
import Semantics.SymbolTable
import Semantics.Visitor



type Analysis = SymbolLookup



analyzeExpr :: Expr -> Visitor Analysis
analyzeExpr (ValueE val) = analyzeValue val
analyzeExpr _ = unknownError


analyzeValue :: Value -> Visitor Analysis
analyzeValue (IntLit _) = return intType
analyzeValue (FltLit _) = return fltType
analyzeValue (ChrLit _) = return chrType
analyzeValue (StrLit _) = return strType
analyzeValue (VarVal name) = symbolData name
analyzeValue (ExprVal expr) = analyzeExpr expr
analyzeValue (CtorVal name args) = do
    nlu <- globalData name
    case nlu of
        Found dta  -> do
            let typ = sdType dta
            newType <- applyArgs args typ
            let newData = dta { sdType = newType }
            return $! (Found newData)
        NotFound _ -> return nlu
analyzeValue (Array _ _) =
    fail "idk how to implement this"


applyArgs :: [Value] -> Type -> Visitor Type
applyArgs _ typ = return $! typ -- TODO
