{-# LANGUAGE LambdaCase #-}

module Typing.Inferable (
    makeInference,
    infer,
    inferTop,
) where

import Prelude hiding (lookup)

import Control.Monad (foldM, forM)
import qualified Data.Array as A (elems)

import AST
import Analysis.Error
import Common.Specifiers
import Data.Table
import Typing.Infer
import Typing.Primitives
import Typing.Scheme
import Typing.Solver
import Typing.Type
import Typing.TypeDecl

import Text.Pretty
import Debug.Trace


-- inferTopLevel :: [Expr] -> 

makeInference :: Inferable a => Table -> a
    -> Either Error Scheme
makeInference env a = case runInfer env (infer a) of
    Left err -> Left err
    Right (typ, cons, _tbl) -> runSolver typ cons


class Inferable a where
    infer :: a -> Infer Type


instance Inferable Type where
    infer (Type name tps) =
        Type name <$> mapM infer tps
    infer TypeVar{} = fresh
    infer (t1 :-> t2) = do
        t1' <- infer t1
        t2' <- infer t2
        tv <- fresh
        constrain t1' (t2' :-> tv)
        return tv
    infer (TupleType types) =
        TupleType <$> mapM infer types
    infer (ArrayType typ) = ArrayType <$> infer typ

instance Inferable Literal where
    infer IntLit{} = return intType
    infer FloatLit{} = return floatType
    infer DoubleLit{} = return doubleType
    infer CharLit{} = return charType
    infer StringLit{} = return stringType

instance Inferable Value where
    infer (Literal lit) = let !_ = traceId ("LITERAL"+|lit) in infer lit
    infer (VarVal name) = searchScopeds name
    infer (Application v1 v2) = do
        t1 <- infer v1
        t2 <- infer v2
        tv <- fresh
        let !_ = traceId ("v1, v2     : "+|", "`seps`[v1,v2])
        let !_ = traceId ("t1, t2, tv : "+|", "`seps`[t1,t2,tv])
        let !_ = traceId ("~~~~~~~~~~~~")
        constrain (t2 :-> tv) t1
        return tv
    infer (CtorCall name) = searchGlobals name
    infer (Lambda ps body) = inNewScope $ do
        pTs <- mapM (const fresh) ps
        tv <- fresh
        let typ = foldTypes pTs tv
        _aftParsT <- applyParams (Param <$> ps) typ
        bT <- infer body
        constrain tv bT
        -- return (aftParsT :-> bT)
        return typ
    infer (Tuple arr) =
        tupleOf <$> mapM infer (A.elems arr)
    infer (Array arr) = do
        tv <- fresh
        arrayOf <$> foldM (\t1 val -> do
            t2 <- infer val
            constrain t1 t2
            return t2
            ) tv arr
    infer (IfElseVal cond tr fa) = do
        cT <- infer cond
        tT <- infer tr
        fT <- infer fa
        constrain cT boolType
        constrain tT fT
        return tT
    infer (MatchVal val cases) = do
        vT <- infer val
        tv <- fresh
        foldM (\prT (ptrn, body) -> do
            ptT <- infer ptrn
            bT <- infer body
            constrain ptT vT
            constrain bT prT
            return bT
            ) tv cases

-- TODO: Consider un-instantiating??
instance Inferable Pattern where
    infer (Param name) =
        funcType <$> pushParam name
    infer Hole{} = fresh
    infer (CtorPtrn name args) = do
        t1 <- searchGlobals name
        -- t2 <- applyPtrns arg args
        t2 <- applyParams args t1
        -- tv <- fresh
        -- constrain t1 (t2 :-> tv)
        -- return (t2 :-> t1)
        return t2
    infer (TuplePtrn args) = tupleOf <$> mapM infer args
    infer (LitPtrn lit) = infer lit
    infer (OrPtrn p1 p2) = do
        t1 <- infer p1
        t2 <- infer p2
        constrain t2 t1
        return t2


-- | Returns `Just t` only if it can guarantee
-- that it will return (i.e. all cases guarantee a
-- return in a `Match`). If no such guarantee can be
-- made, it will return `Nothing`.
inferStmt :: Stmt -> Infer (Maybe Type)
inferStmt (IfElse cond trueBody falseBody) = do
    cT <- infer cond
    constrain cT boolType
    etb <- allowJumpsIn $ inferStmt trueBody
    efb <- allowJumpsIn $ inferStmt falseBody
    mergeStmts etb efb
inferStmt (Loop init' cond iter body) = do
    inferStmt init'
    case cond of
        ValStmt val -> do
            cT <- infer val
            constrain cT boolType
        _ -> throw $ OtherError
            "invalid condition expression"
    inferStmt iter
    allowJumpsIn $ inferStmt body
    return Nothing
inferStmt (Match val cases) = do
    vT <- infer val
    allowJumpsIn $ inferCases vT cases
inferStmt (NewVar mut name (TypeDecl _ typ) val) = do
    -- 'cleanses' the type
    typ' <- infer typ
    pushScoped mut name typ'
    vT <- infer val
    constrain vT typ'
    return Nothing
inferStmt (Reassignment name val) = do
    vT <- infer val
    nT <- findScoped name
    constrain vT nT
    return Nothing
inferStmt (Return val) = Just <$> infer val
inferStmt (ValStmt val) = do
    infer val
    return Nothing
inferStmt (Compound body) =
    foldM (\b stmt -> do
        i <- inferStmt stmt
        mergeStmts b i
        ) Nothing body
inferStmt Break = do
    legal <- gets jumpAllowed
    if not legal then
        throw $ OtherError "illegal `break`"
    else
        return Nothing
inferStmt Continue = do
    legal <- gets jumpAllowed
    if not legal then
        throw $ OtherError "illegal `continue`"
    else
        return Nothing
inferStmt NullStmt = return Nothing

inferCases :: Type -> [MatchCase] -> Infer (Maybe Type)
inferCases _ [] = return Nothing
inferCases vT cases =
    foldM (\prev (Case ptrn body) -> do
        ptT <- infer ptrn
        constrain ptT vT
        mbT <- inferStmt body
        mergeStmts prev mbT
    ) Nothing cases

inferTop :: Expr -> Infer ()
inferTop (FuncDecl _pur vis name (TypeDecl _ typ)) = do
    pushGlobal name vis typ
    return ()
inferTop (FuncDef name params body) = inNewScope $ do
    sT <- searchGlobals name
    aftParsT <- applyParams params sT
    bT <- inferStmt body >>= \case
        Nothing -> throw (MissingReturn name)
        Just typ -> return typ
    tv <- fresh
    -- Does this get reversed?
    constrain (aftParsT :-> tv) bT
    constrain (aftParsT :-> bT) sT
    pushUndefGlobal name (aftParsT :-> bT)
    return ()
inferTop (DataDef vis name tps ctors) = do
    tvs <- mapM (const fresh) tps
    let typ = Type name tvs
    let ctorNames = map ctorName ctors
    mapM_ (inferCtor typ) ctors
    pushData name vis typ ctorNames
    return ()
inferTop (TraitDecl _vis _ctx _name _tps fns) = do
    mapM_ inferTop fns
inferTop (TraitImpl _ctx _name _types fns) = do
    mapM_ inferTop fns
inferTop (TypeAlias _vis _alias _typ) = do
    return ()

-- |Creates globals for the `Ctor` and, if applicable,
-- its fields.
inferCtor :: Type -> Ctor -> Infer ()
inferCtor pT (Record name vis fields) = do
    types <- forM fields $ \(Field name' typ) -> do
        pushGlobal name' vis typ
        return $! typ
    let typ = foldTypes types pT
    pushGlobal name vis typ
    return ()
inferCtor pT (SumType name vis types) = do
    let typ = foldTypes types pT
    pushGlobal name vis typ
    return ()

-- if both bodies guarantee a return, then this can
-- as well. otherwise it cannot be guaranteed.
mergeStmts :: Maybe Type -> Maybe Type -> Infer (Maybe Type)
mergeStmts Nothing Nothing = return Nothing
mergeStmts (Just typ) Nothing = return (Just typ)
mergeStmts Nothing (Just typ) = return (Just typ)
mergeStmts (Just t1) (Just t2) = do
    constrain t2 t1
    return (Just t2)

-- applyPtrns :: Pattern -> [Pattern] -> Infer Type
-- applyPtrns val [] = infer val
-- applyPtrns v1 (v2:vs) = do
--     t1 <- infer v1
--     t2 <- applyPtrns v2 vs
--     tv <- fresh
--     let typ = t2 :-> tv
--     constrain t1 typ
--     return typ

applyParams :: [Pattern] -> Type -> Infer Type
applyParams [] typ = return typ
applyParams (p:ps) (typ :-> types) = do
    typ' <- go p
    constrain typ' typ
    applyParams ps types
    where
        -- this is different from just inferring b/c
        -- it takes `typ` into account
        go ptrn = case ptrn of
            Param name -> do
                pushScoped Imut name typ
                return typ
            Hole _ -> return typ
            CtorPtrn name args -> do
                dType <- searchGlobals name
                typ' <- applyParams args dType
                constrain typ' typ
                return typ'
            TuplePtrn ptrns' -> do
                -- not quite right...
                types' <- mapM (const fresh) ptrns'
                let typ' = foldr1 (:->) types'
                typ'' <- applyParams ptrns' typ'
                constrain typ'' typ
                return typ''
            LitPtrn lit -> infer lit
            OrPtrn p1 p2 -> do
                t1 <- go p1
                t2 <- go p2
                constrain t1 t2
                return t2
applyParams pars typ = do
    tvs <- mapM (const fresh) pars
    tv <- fresh
    throw (TypeMismatch (typ :-> foldTypes tvs tv) typ)
