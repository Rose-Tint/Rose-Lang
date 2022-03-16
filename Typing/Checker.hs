module Typing.Checker where

import Control.Monad ((<$!>), foldM)
import Data.Maybe (fromMaybe, fromJust)

import Analyzer.Analyzer
import Analyzer.Error
import Analyzer.Prims
import Analyzer.SymbolTable
import Parser.Data hiding (Type)
import SymbolTable
import Typing.Types



class Checker a where
    infer :: a -> Analyzer Type
    check :: a -> Type -> Analyzer Bool
    check a expected = do
        typ <- infer a
        return $! (typ == expected)



instance Checker Type where
    infer t@(Delayed _) = fromMaybe t <$!> peekExpType
    infer t = return $! t


instance Checker Value where
    infer (IntLit _) = return intLitType
    infer (FltLit _) = return fltLitType
    infer (ChrLit _) = return chrLitType
    infer (StrLit _) = return strLitType
    infer (FuncCall name args) = do
        dta <- searchScopeds name
        withExpType (sdType dta) $!
            apply (sdType dta) args
    infer (CtorVal name args) = do
        dta <- searchGlobals name
        withExpType (sdType dta) $!
            apply (sdType dta) args
    infer (ExprVal expr) = infer expr
    infer (Array _ []) = throw (OtherError "empty array")
    infer (Array _ (x:xs)) = do
        xT <- infer x
        pushExpType xT
        chk <- catch $! foldM
            (\b a -> do
                aT <- infer $! a
                chk <- check aT xT
                if chk then
                    return $! b
                else
                    throw $! TypeMismatch Nothing aT xT
            ) xT xs
        popExpType
        case chk of
            Nothing -> throw $! TypeMismatch1 Nothing xT
            Just t -> return $! arrayLitOf t


instance Checker Expr where
    -- i do not know how to handle this (an expression
    -- without a type) without adding an entire new
    -- sub-function to Analyzer
    infer (ValueE val) = infer val
    infer (ModImport vis var) = do
        addImport vis var
        fail "unhandled Expr (ModImport)"
    -- infer (FuncTypeDecl pur vis name cons typs) = do
    --     pushDefinition name
    --     dta <- searchGlobals name
    infer _ = fail "`Checker Expr` not fully implemented"


{-
ModImport -- WIP
    FuncTypeDecl
        exprPurity :: Purity,
        exprVisib :: Visibility,
        exprName :: Variable,
        exprCons :: [Constraint],
        exprType :: [Type]
    }
    | FuncDef {
        exprName :: Variable,
        exprPars :: [Value],
        exprBody :: Body
    }
    | DataDef {
        exprVisib :: Visibility,
        exprName :: Variable,
        exprTypePars :: [Variable],
        exprCtors :: [DataCtor]
    }
    | IfElse {
        exprClause :: Value,
        exprTrue :: Body,
        exprFalse :: Body
    }
    | Pattern {
        exprValue :: Value,
        exprCases :: [(Value, Body)]
    }
    | Loop {
        exprInit :: Maybe Expr,
        exprCond :: Value,
        exprIter :: Maybe Expr,
        exprBody :: Body
    }
    | TraitDecl {
        exprVisib :: Visibility,
        exprCons :: [Constraint],
        exprName :: Variable,
        exprTypeVar :: Variable,
        exprFuncs :: [Expr]
    }
    | TraitImpl {
        exprName :: Variable,
        exprCons :: [Constraint],
        exprTraitType :: Maybe Type,
        exprDefs :: [Expr]
    }
    | NewVar Mutability Type Variable Value
    | Reassign Variable Value
    | Return Value
-}



apply :: Type -> [Value] -> Analyzer Type
apply ft [] = return ft
apply ft (val:vals) = do
    valT <- infer val
    expT <- peekExpType
    areSame <- case expT of
        Nothing -> return True
        Just t -> check valT t
    if areSame then
        apply ft vals
    else
        -- areSame will be True if expT is Nothing,
        -- so fromJust is safe here
        throw (TypeMismatch Nothing valT (fromJust expT))


checkAll :: (Checker a) => [a] -> Analyzer Bool
checkAll [] = return True
checkAll (x:xs) = do
    xT <- infer x
    foldM (\b a -> if not b then return b else do
        check a xT) True xs


expect :: (Checker a) => a -> Type -> Analyzer Bool
expect a t = do
    typ <- infer a
    check typ t
