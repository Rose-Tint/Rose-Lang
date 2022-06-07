{-# LANGUAGE LambdaCase #-}

module Typing.Inferable (
    makeInference,
    infer,
    inferTopLevel,
) where

import Prelude hiding (lookup)

import Control.Monad (foldM, unless)
import qualified Data.Array as A (elems)

import AST
import Analysis.Error
import Data.Table
import Typing.Infer
import Typing.Primitives
import Typing.Scheme
import Typing.Solver
import Typing.Type


inferTopLevel :: Table -> [Expr] -> (Table, [ErrInfo])
inferTopLevel tbl [] = (tbl, [])
inferTopLevel tbl exprs =
    let inf = mapM_ inferTop exprs
        Inf _ _cons tbl' errs = runInfer tbl inf
    in (tbl', errs)

makeInference :: Table -> Infer Type -> Either [ErrInfo] Scheme
makeInference tbl inf =
    let Inf typ cons _tbl errs = runInfer tbl inf
    in case runSolver typ cons of
        Left err -> Left (err:errs)
        Right scheme -> case errs of
            [] -> Right scheme
            _ -> Left errs


class Inferable a where
    infer :: a -> Infer Type


instance Inferable Literal where
    infer lit = do
        updatePos lit
        return $ case lit of
            IntLit{} -> intType
            FloatLit{} -> floatType
            DoubleLit{} -> doubleType
            CharLit{} -> charType
            StringLit{} -> stringType

instance Inferable Value where
    infer (Literal lit) = infer lit
    {- ^
    x : σ ∈ Γ
    ---------
    Γ ⊢ x : σ
    -}
    infer (VarVal name) = do
        updatePos name
        searchScopeds name
    {- ^
    Γ ⊢ e1 : τ1 -> τ2    Γ ⊢ e2 : τ1
    --------------------------------
    Γ ⊢ e1 e2 : τ2
    -}
    infer (Application v1 v2) = do
        t1 <- infer v1
        t2 <- infer v2
        tv <- fresh
        constrain (t2 :-> tv) t1
        return tv
    {- ^
    x: σ ∈ Γ
    ---------
    Γ ⊢ x : σ
    -}
    infer (CtorCall name) = do
        updatePos name
        searchGlobals name
    {- ^
    Γ, x : τ1 ⊢ e : τ2
    --------------------
    Γ ⊢ λx.e : τ1 -> τ2
    -}
    infer (Lambda x e) = do
        updatePos x
        tv <- fresh
        ty <- inNewScope $ do
            pushScoped x tv
            infer e
        return (tv :-> ty)
    {- ^
    Γ ⊢ e1 : τ1  ...  Γ ⊢ en : τn
    ---------------------------------
    Γ ⊢ (e1, ..., en) : (τ1, ..., τn)
    -}
    infer (Tuple arr) = do
        ts <- mapM infer (A.elems arr)
        return (tupleOf ts)
    {- ^
    Γ ⊢ e1 : τ1  ...  Γ ⊢ en : τ1
    ------------------------------
    Γ ⊢ [e1, ..., en] : [τ1]
    -}
    infer (Array arr) = do
        tv <- fresh
        t1 <- foldM (\ t1 val -> do
            t2 <- infer val
            constrain t1 t2
            return t2
            ) tv arr
        return (arrayOf t1)
    {- ^
    x : σ ∉ Γ    Γ, x : σ ⊢ e1 : σ    Γ, x : σ ⊢ e2 : τ
    ----------------------------------------------------
    Γ ⊢ let x = e1 in e2 : τ
    -}
    infer (LetIn x e1 e2) = do
        updatePos x
        typ <- inNewScope $ do
            tv <- fresh
            pushNewScoped x tv
            t1 <- infer e1
            constrain t1 tv
            t2 <- infer e2
            return t2
        return typ
    {- ^
    Γ ⊢ e1 : Bool    Γ ⊢ e2 : τ1    Γ ⊢ e3 : τ1
    -------------------------------------------
    Γ ⊢ if e1 then e2 else e3 : τ1
    -}
    infer (IfElseVal e1 e2 e3) = do
        condT <- infer e1
        t2 <- infer e2
        t3 <- infer e3
        constrain boolType condT
        constrain t2 t3
        constrain t3 t2
        return t3
    {- ^
    Γ ⊢ e1 : τ1    (Γ ⊢ e2 : τ1    Γ ⊢ e3 : τ2
    ...  Γ ⊢ en : τ1    Γ ⊢ en' : τ2)
    -----------------------------------------------
    Γ ⊢ match e1 { (e2 => e2' ... en => en') } : τ2
    -}
    infer (MatchVal e1 cases) = do
        t1 <- infer e1
        tv <- fresh
        t2 <- foldM (\ t2 (ValCase en en') -> do
            ptrnT <- infer en
            bodyT <- infer en'
            constrain ptrnT t1
            constrain t2 bodyT
            return bodyT
            ) tv cases
        return t2

-- TODO: Consider un-instantiating??
instance Inferable Pattern where
    {- ^
    x : σ ∉ Γ
    ------------
    Γ, x : σ ⊢ x : σ
    -}
    infer (Param name) = do
        updatePos name
        tv <- fresh
        pushScoped name tv
        return tv
    {- ^
    ---------
    Γ ⊢ _ : τ
    -}
    infer (Hole p) = do
        updatePos p
        tv <- fresh
        return tv
    {- ^
    x : (τ1 -> ... -> τn -> σ) ∈ Γ
    Γ |- e1 : τ1  ...  Γ |- en : τn
    -----------------------------------
    Γ ⊢ [x e1 ... en] : σ
    -}
    infer (CtorPtrn name args) = do
        updatePos name
        t1 <- searchGlobals name
        t2 <- applyParams args t1
        constrain t1 t2
        return t2
    {- ^
    Γ ⊢ e1 : τ1  ...  Γ ⊢ en : τn
    -----------------------------------
    Γ ⊢ [(e1, ..., e2)] : (τ1, ..., τn)
    -}
    infer (TuplePtrn es) = do
        ts <- mapM infer es
        return (tupleOf ts)
    infer (LitPtrn lit) = infer lit


-- | Returns `Just t` only if it can guarantee
-- that it will return (i.e. all cases guarantee a
-- return in a `Match`). If no such guarantee can be
-- made, it will return `Nothing`.
inferStmt :: Stmt -> Infer (Maybe Type)
{- ^
Γ |- e1 : Bool    Γ |- e2 : τ1    Γ |- e3 : τ2
-------------------------------------------------
Γ |- if e1 e2 else e3 : `mergeStmts` τ2 τ3
-}
inferStmt (IfElse e1 e2 e3) = do
    condT <- infer e1
    t2 <- allowJumpsIn (inferStmt e2)
    t3 <- allowJumpsIn (inferStmt e3)
    constrain boolType condT
    _t3' <- mergeStmts t2 t3
    return Nothing
    -- return t3'
{- ^
Γ |- e1 : τ1    Γ, e1, e2 |- e2 : Bool
Γ, e1 |- e3 : τ2    Γ, e1, e2, e3 |- e4 : τ3
----
Γ |- loop (e1; e2; e3) e4 : `Nothing`
-}
inferStmt (Loop init' cond iter body) = do
    inNewScope $ do
        inferStmt init'
        inferCond cond
        inferStmt iter
        allowJumpsIn (inferStmt body)
    return Nothing
    where
        inferCond cond' = case cond' of
            ValStmt val -> do
                cT <- infer val
                constrain boolType cT
            _ -> do
                throw $ OtherError
                    "invalid condition expression"
                return ()
{- ^
Γ |- e1 : τ1    Γ |- e2 : τ1    Γ, e2 |- e2' : τ2
...  Γ |- en : τ1    Γ, en |- en' : τ2
---------------------------------------------------
Γ |- match e1 { e2 '->' e2'  ...  en '->' en' } : τ2
-}
inferStmt (Match e1 es) = do
    t1 <- infer e1
    _t2 <- allowJumpsIn (inferCases t1 es)
    return Nothing
    -- return (Just t2)
{- ^
x : σ ∉ Γ    Γ, x : σ |- e : σ
--------------------------------------
Γ, x : τ |- let x = e : `Nothing` -- σ
-}
inferStmt (NewVar name _tyd e) = do
    -- tv <- fresh
    -- pushNewScoped name tv
    -- not ideal (doesn't allow for recursion), but
    -- this way is simpler and true-er to the rule
    -- above
    typ <- inNewScope (infer e)
    pushNewScoped name typ
    return Nothing
    -- return (Just typ)
{- ^
x : σ ∈ Γ    Γ |- e : σ
-----------------------------
Γ |- x = e : `Nothing` -- σ
-}
inferStmt (Reassignment x e) = do
    t1 <- findScoped x
    t2 <- infer e
    constrain t1 t2
    return Nothing
    -- return (Just t2)
{- ^
Γ |- e : τ
-----------------
Γ |- return e : τ
-}
inferStmt (Return val) = do
    typ <- infer val
    return (Just typ)
inferStmt (ValStmt val) = do
    _typ <- infer val
    return Nothing
    -- return (Just typ)
inferStmt (Compound body) =
    foldM (\b stmt -> do
        i <- inferStmt stmt
        mergeStmts b i
        ) Nothing body
inferStmt Break = do
    legal <- gets jumpAllowed
    unless legal $
        throw (OtherError "illegal `break`")
    return Nothing
inferStmt Continue = do
    legal <- gets jumpAllowed
    unless legal $
        throw (OtherError "illegal `continue`")
    return Nothing
inferStmt NullStmt = return Nothing

inferCases :: Type -> [StmtCase] -> Infer (Maybe Type)
inferCases _ [] = return Nothing
inferCases t1 [StmtCase ptrn body] = do
    ptrnT <- infer ptrn
    constrain t1 ptrnT
    mT <- inferStmt body
    return mT
inferCases t1 (StmtCase ptrn body:cases) = do
    ptrnT <- infer ptrn
    constrain t1 ptrnT
    mT1 <- inferStmt body
    mT2 <- inferCases t1 cases
    -- can't use `mergeStmts` bc ALL cases must return
    case (mT1, mT2) of
        (Just t2, Just t2') -> do
            constrain t1 t2
            return (Just t2')
        _ -> return Nothing

inferTop :: Expr -> Infer ()
inferTop (FuncDecl _pur name typ) = do
    pushGlobal name typ
    return ()
{- ^
Γ ⊢ f : σ    (Γ ⊢ x1 : τ1 ... Γ ⊢ xn : τn)
Γ, f, x1, ..., xn : σ ⊢ e : τ'
---------------------------------------------------
Γ, σ ⊢ f x1 ... xn e : τ1 -> ... -> τn -> τ'
-}
inferTop (FuncDef name params body) = inNewScope $ do
    sT <- searchGlobals name
    aftParsT <- applyParams params sT
    bT <- inferStmt body >>= \case
        Nothing -> do
            throw (MissingReturn name)
            tv <- fresh
            return tv
        Just typ -> return typ
    let typ = aftParsT :-> bT
    constrain sT typ
    pushGlobal name typ
    return ()
inferTop (DataDef name tps ctors) = do
    updatePos name
    tvs <- mapM (const fresh) tps
    let typ = TypeCon name tvs
    let ctorNames = map ctorName ctors
    mapM_ (inferCtor typ) ctors
    pushData name typ ctorNames
    return ()
inferTop (TraitDecl _ctx _name _tps fns) = do
    mapM_ inferTop fns
inferTop (TraitImpl _ctx _name _types _fns) = do
    return ()
    -- mapM_ inferTop fns

-- |Creates globals for the `Ctor` and, if applicable,
-- its fields.
inferCtor :: Type -> Ctor -> Infer ()
{-# INLINE inferCtor #-}
inferCtor pT (SumType name types) = do
    updatePos name
    let typ = foldTypes types pT
    pushGlobal name typ
    return ()

-- if both bodies guarantee a return, then this can
-- as well. otherwise it cannot be guaranteed.
mergeStmts :: Maybe Type -> Maybe Type -> Infer (Maybe Type)
{-# INLINE mergeStmts #-}
mergeStmts Nothing Nothing = return Nothing
mergeStmts (Just typ) Nothing = return (Just typ)
mergeStmts Nothing (Just typ) = return (Just typ)
mergeStmts (Just t1) (Just t2) = do
    constrain t1 t2
    return (Just t2)

applyParams :: [Pattern] -> Type -> Infer Type
{-# INLINE applyParams #-}
applyParams ptrns typ = do
    pTs <- mapM infer ptrns
    tv <- fresh
    constrain typ (foldTypes pTs tv)
    return tv
-- applyParams [] typ = return typ
-- applyParams (p:ps) (typ :-> types) = do
--     typ' <- go p
--     constrain typ typ'
--     applyParams ps types
--     where
--         -- this is different from just inferring b/c
--         -- it takes `typ` into account
--         go ptrn = case ptrn of
--             Param name -> do
--                 updatePos name
--                 pushScoped name typ
--                 return typ
--             Hole pos -> do
--                 updatePos pos
--                 return typ
--             CtorPtrn name args -> do
--                 updatePos name
--                 dType <- searchGlobals name
--                 typ' <- applyParams args dType
--                 constrain dType typ'
--                 return typ'
--             TuplePtrn ptrns' -> do
--                 -- not quite right...
--                 types' <- mapM (const fresh) ptrns'
--                 let typ' = foldr1 (:->) types'
--                 typ'' <- applyParams ptrns' typ'
--                 return typ''
--             LitPtrn lit -> do
--                 updatePos lit
--                 typ' <- infer lit
--                 return typ'
-- applyParams pars typ = do
--     tvs <- mapM (const fresh) pars
--     tv <- fresh
--     let typ' = typ :-> foldTypes tvs tv
--     throw (TypeMismatch typ' typ)
--     return typ'
