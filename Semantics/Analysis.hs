module Semantics.Analysis where

import Control.Monad (when)

import Parser.Data
import Semantics.Error (Error(..))
import Semantics.SymbolTable
import Semantics.Visitor



data Analysis = Analysis SymbolTable Error



-- Adds datatypes and top-level functions to the namespace
preprocess :: [Expr] -> Visitor ()
preprocess [] = return ()
preprocess (expr:exprs) = case expr of
    FuncTypeDecl pur vis name _ typ -> do
        pushGlobal name
            (Symbol pur vis (NonTermType typ) name "")
        prepRest
    DataDef vis name tps ctors -> do
        let tpTypes = fmap (\n ->
                TerminalType (TypeParam n) []) tps
        let thisType = TerminalType (RealType name) tpTypes
        pushCtors thisType ctors
        pushType name (Symbol Pure vis thisType name "")
        prepRest
    _ -> prepRest
    where
        prepRest = preprocess exprs
        pushCtors _ [] = return ()
        pushCtors typ (ctor:ctors) = do
            pushGlobal (ctorName ctor) (Symbol
                    Pure (ctorVisib ctor)
                    (NonTermType (ctorTypes ctor ++ [typ]))
                    (ctorName ctor) ""
                )
            pushCtors typ ctors


{-
TO ANALYZE:
    Module Imports (SEE COMMENT)
    If-Then-Elses (WIP)
    Pattern Matching
    Loops
    Trait Declarations
    Trait Implementations
-}


analyzeExpr :: Expr -> Visitor (Maybe SymbolData)
analyzeExpr (ValueE val) = Just <$> analyzeValue val
-- analyzeExpr (ModImport _) = -- i need IO for this...
-- analyzeExpr (FuncTypeDecl pur vis name cons tps) = return Nothing
analyzeExpr (FuncTypeDecl _ _ _ _ _) = return Nothing
analyzeExpr (FuncDef name pars bdy) = do
    pushScope
    dta <- globalData name
    setPurity (Just $! sdPurity dta)
    newType <- pushParams (sdType dta) pars
    pushExpType newType
    retAnl <- analyzeBody bdy
    case retAnl of
        Nothing -> otherError "No return in function body"
        Just retDta -> let retType = sdType retDta in do
            popScope; popExpType
            if newType == retType then
                return $! Just (dta { sdType = newType })
            else
                typeMismatch retType newType
analyzeExpr (DataDef _ _ _ _) = return Nothing
analyzeExpr (IfElse cnd tb fb) = do
    pushExpType boolType
    cndType <- sdType <$> analyzeValue cnd
    popExpType
    -- loop condition must be a Boolean
    if cndType == boolType then do
        tDta <- analyzeBody tb
        fDta <- analyzeBody fb
        case (tDta, fDta) of
            (Nothing, _) -> return fDta
            (_, Nothing) -> return tDta
            (Just tDta', Just fDta') -> do
                let (tType, fType) =
                        (sdType tDta', sdType fDta')
                when (tType /= fType)
                    (typeMismatch tType fType
                    >> return ())
                return Nothing
    else
        typeMismatch boolType cndType
analyzeExpr (Pattern _ []) = return Nothing
analyzeExpr (Pattern mat pats) = do
    valType <- sdType <$> analyzeValue mat
    cases <- verifyCaseTypes valType (fmap fst pats)
    if not cases then return Nothing else
        bodiesType (fmap snd pats)
    where
        verifyCaseTypes :: Type -> [Value] -> Visitor Bool
        verifyCaseTypes _ [] = return True
        verifyCaseTypes typ (val:vals) = do
            valType <- sdType <$> analyzeValue val
            if valType /= typ then
                typeMismatch typ valType
                -- verifyCaseTypes typ vals
                -- return False
            else
                verifyCaseTypes typ vals
        bodiesType :: [Body] -> Visitor (Maybe SymbolData)
        bodiesType bdys =
            analyzeBody (head bdys) -- temp
analyzeExpr (NewVar mut typ name val) = do
    st <- symbolTable
    case lookupSymbol st name of
        Found sym -> redefinition (sdVar sym) name
        NotFound _ -> do
            valDta <- analyzeValue val
            let valType = sdType valDta
            if typ == valType then do
                pushScoped name mut typ
                return Nothing
            else
                typeMismatch typ valType
analyzeExpr (FuncCall name args) = do
    _ <- do
        et <- typeFromValues args
        case et of
            Nothing -> indetType name
            Just typ -> pushExpType typ
    dta <- symbolData name
    newType <- applyArgs args (sdType dta)
    popExpType
    return $! Just (dta { sdType = newType })
analyzeExpr (Reassign name val) = do
    varType <- sdType <$> symbolData name
    valType <- sdType <$> analyzeValue val
    if varType == valType then
        return Nothing
    else
        typeMismatch varType valType
analyzeExpr (Return val) = Just <$> analyzeValue val
analyzeExpr (TraitDecl _ _ _ _ _) = return Nothing
analyzeExpr (TraitImpl _ _ _ _) = return Nothing
analyzeExpr _ = otherError "UN-IMPLEMENTED EXPRESSION"


analyzeBody :: [Expr] -> Visitor (Maybe SymbolData)
analyzeBody [] = return Nothing
analyzeBody [expr] = analyzeExpr expr
analyzeBody (expr:exprs) = case expr of
    Return val -> Just <$> analyzeValue val
    _ -> analyzeBody exprs


pushParams :: Type -> [Value] -> Visitor Type
pushParams typ@(TerminalType _ _) [] = return  typ
pushParams (TerminalType _ _) _ =
    otherError "unexpected terminal type"
pushParams (NonTermType []) _ =
    otherError "unexpected empty non-terminal type"
pushParams (NonTermType [x]) [] = return (simplifyType x)
pushParams (NonTermType (typ:typs)) (p:ps) = do
    pushExpType typ
    case p of
        VarVal name -> do
            pushScoped name Pure typ
            pushRest
        CtorVal name args -> do
            dta <- globalData name
            pushParams (sdType dta) args
            pushRest
        ExprVal _ -> otherError "Expression cannot be in pattern"
        _ -> do
            dta <- analyzeValue p
            if typ == sdType dta then
                pushRest
            else
                typeMismatch typ (sdType dta)
    where
        pushRest = popExpType >>
            pushParams (NonTermType typs) ps
pushParams typ [] = return typ


analyzeValue :: Value -> Visitor SymbolData
analyzeValue (IntLit _) = return $! primTypeData "Int"
analyzeValue (FltLit _) = return $! primTypeData "Float"
analyzeValue (ChrLit _) = return $! primTypeData "Char"
analyzeValue (StrLit _) = return $! (primTypeData "Array")
    { sdType = sdType (primTypeData "Char") }
analyzeValue (VarVal name) = symbolData name
analyzeValue (ExprVal expr) = do
    anl <- analyzeExpr expr
    case anl of
        Just dta -> return $! dta
        Nothing -> otherError "expression does not return value"
analyzeValue (CtorVal name args) = do
    dta <- globalData name
    newType <- applyArgs args (sdType dta)
    return $! dta { sdType = newType }
analyzeValue (Array _ _) = -- primTypeData "Array"
    fail "idk how to implement this"


typeFromValues :: [Value] -> Visitor (Maybe Type)
typeFromValues [] = expectedType
typeFromValues [val] = do
    expType <- expectedType
    valType <- sdType <$> analyzeValue val
    case expType of
        Nothing ->
            return $! Just valType
        Just typ ->
            return $! Just (NonTermType [valType, typ])
typeFromValues (val:vals) = do
    valType <- sdType <$> analyzeValue val
    rest <- typeFromValues vals
    return $! case rest of
        Nothing -> Just valType
        Just (NonTermType valTypes) ->
            Just (simplifyType $ NonTermType (valType:valTypes))
        Just typ -> Just (NonTermType [valType, typ])


applyArgs :: [Value] -> Type -> Visitor Type
applyArgs (a:as) (NonTermType (t:ts)) = do
    pushExpType t
    _ <- case a of
        VarVal name -> do
            matchSymToType name t
            return ()
        _ -> do
            aType <- sdType <$> analyzeValue a
            when (aType /= t)
                (typeMismatch t aType)
    popExpType
    applyArgs as (simplifyType (NonTermType ts))
applyArgs _ typ = return typ
    
