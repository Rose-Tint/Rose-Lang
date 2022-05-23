{-# LANGUAGE LambdaCase #-}

module Typing.Inferable (
    makeInference,
    infer,
    inferTopLevel,
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


inferTopLevel :: [Expr] -> Either ErrInfo Table
inferTopLevel [] = Right emptyTable
inferTopLevel exprs = case runInfer emptyTable inf of
    Left err -> Left err
    Right ((), _cons, tbl) -> Right tbl
    where
        inf = mapM_ inferTop exprs

makeInference :: Table -> Infer Type
    -> Either ErrInfo Scheme
makeInference tbl inf = case runInfer tbl inf of
    Left err -> Left err
    Right (typ, cons, _tbl) -> runSolver typ cons


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
    -- if bound:
    x : σ ∈ Γ
    ------------
    Γ ⊢ x : σ

    else (if unbound):
    x : σ ∉ Γ
    ------------
    Γ, x ⊢ x : σ
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
        constrain t1 (t2 :-> tv)
        return tv
    {- ^
    x: σ ∈ Γ
    ---------
    Γ ⊢ x : σ
    -}
    infer (CtorCall name) = do
        updatePos name
        searchGlobals name
    infer (Lambda [] _) = throw $ OtherError
        "Missing parameters from lambda expression"
    {- ^
    Γ, x : τ1 ⊢ e : τ2
    --------------------
    Γ ⊢ λx. e : τ1 -> τ2
    -}
    infer (Lambda [x] e) = do
        updatePos x
        tv <- fresh
        ty <- inNewScope $ do
            pushScoped Imut x tv
            infer e
        return (tv :-> ty)
    {- ^
    Γ, (x1 : τ1, ..., xn : τn) ⊢ e : τ(n+1)
    ---------------------------------------------
    Γ ⊢ λ(x1 ... xn) => e : (τ1 -> ... -> τ(n+1))
    -}
    infer (Lambda (x:xs) body) = do
        updatePos x
        tv <- fresh
        ty <- inNewScope $ do
            pushScoped Imut x tv
            -- safe to do bc of prev pattern.
            -- it's basically just desugaring.
            infer (Lambda xs body)
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
    Γ ⊢ e1 : Bool    Γ ⊢ e2 : τ1    Γ ⊢ e3 : τ1
    -------------------------------------------
    Γ ⊢ if e1 then e2 else e3 : τ1
    -}
    infer (IfElseVal e1 e2 e3) = do
        condT <- infer e1
        t2 <- infer e2
        t3 <- infer e3
        constrain condT boolType
        constrain t2 t3
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
        t2 <- foldM (\t2 (en, en') -> do
            ptrnT <- infer en
            bodyT <- infer en'
            constrain ptrnT t1
            constrain bodyT t2
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
        pushScoped Imut name tv
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
    x : (\t1 -> ... -> \tn -> σ) ∈ Γ
    \G |- e1 : \t1  ...  \G |- en : \tn
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
    {- ^
    Γ ⊢ e1 : τ1  ...  Γ ⊢ e2 : τ1
    -----------------------------
    Γ ⊢ [e1, e2] : τ1
    -}
    infer (OrPtrn e1 e2) = do
        t1 <- infer e1
        t2 <- infer e2
        constrain t1 t2
        return t2


-- | Returns `Just t` only if it can guarantee
-- that it will return (i.e. all cases guarantee a
-- return in a `Match`). If no such guarantee can be
-- made, it will return `Nothing`.
inferStmt :: Stmt -> Infer (Maybe Type)
{- ^
\G |- e1 : Bool    \G |- e2 : \t1    \G |- e3 : \t2
-------------------------------------------------
\G |- if e1 e2 else e3 : `mergeStmts` \t2 \t3
-}
inferStmt (IfElse e1 e2 e3) = do
    condT <- infer e1
    t2 <- allowJumpsIn (inferStmt e2)
    t3 <- allowJumpsIn (inferStmt e3)
    constrain condT boolType
    _t3' <- mergeStmts t2 t3
    return Nothing
    -- return t3'
{- ^
\G |- e1 : \t1    \G, e1, e2 |- e2 : Bool
\G, e1 |- e3 : \t2    \G, e1, e2, e3 |- e4 : \t3
----
\G |- loop (e1; e2; e3) e4 : `Nothing`
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
                constrain cT boolType
            _ -> throw $ OtherError
                "invalid condition expression"
{- ^
\G |- e1 : \t1    \G |- e2 : \t1    \G, e2 |- e2' : \t2
...  \G |- en : \t1    \G, en |- en' : \t2
---------------------------------------------------
\G |- match e1 { e2 '->' e2'  ...  en '->' en' } : \t2
-}
inferStmt (Match e1 es) = do
    t1 <- infer e1
    _t2 <- allowJumpsIn (inferCases t1 es)
    return Nothing
    -- return (Just t2)
{- ^
x : \s \nel \G    \G, x : \s |- e : \s
--------------------------------------
\G, x : \t |- let x = e : `Nothing` -- \s
-}
inferStmt (NewVar mut name _tyd e) = do
    -- tv <- fresh
    -- pushNewScoped name tv
    -- not ideal (doesn't allow for recursion), but
    -- this way is simpler and true-er to the rule
    -- above
    typ <- inNewScope (infer e)
    pushNewScoped mut name typ
    return Nothing
    -- return (Just typ)
{- ^
x : \s \el \G    \G |- e : \s
-----------------------------
\G |- x = e : `Nothing` -- \s
-}
inferStmt (Reassignment x e) = do
    t1 <- findScoped x
    t2 <- infer e
    constrain t1 t2
    return Nothing
    -- return (Just t2)
{- ^
\G |- e : \t
------------
\G |- return e : \t
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
inferCases t1 [Case ptrn body] = do
    ptrnT <- infer ptrn
    constrain ptrnT t1
    mT <- inferStmt body
    return mT
inferCases t1 (Case ptrn body:cases) = do
    ptrnT <- infer ptrn
    constrain ptrnT t1
    mT1 <- inferStmt body
    mT2 <- inferCases t1 cases
    -- can't use `mergeStmts` bc ALL cases must return
    case (mT1, mT2) of
        (Just t2, Just t2') -> do
            constrain t1 t2
            return (Just t2')
        _ -> return Nothing

inferTop :: Expr -> Infer ()
inferTop (FuncDecl _pur vis name (TypeDecl _ typ)) = do
    pushGlobal name vis typ
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
        Nothing -> throw (MissingReturn name)
        Just typ -> return typ
    tv <- fresh
    let typ = aftParsT :-> bT
    -- Does this get reversed?
    constrain (aftParsT :-> tv) bT
    -- constrain typ sT
    pushUndefGlobal name typ
    return ()
inferTop (DataDef vis name tps ctors) = do
    updatePos name
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
    updatePos name
    types <- forM fields $ \(Field name' typ) -> do
        pushGlobal name' vis typ
        return $! typ
    let typ = foldTypes types pT
    pushGlobal name vis typ
    return ()
inferCtor pT (SumType name vis types) = do
    updatePos name
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
                updatePos name
                pushScoped Imut name typ
                return typ
            Hole pos -> do
                updatePos pos
                return typ
            CtorPtrn name args -> do
                updatePos name
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
            LitPtrn lit -> do
                updatePos lit
                typ' <- infer lit
                constrain typ typ'
                return typ'
            OrPtrn p1 p2 -> do
                t1 <- go p1
                t2 <- go p2
                constrain t1 t2
                return t2
applyParams pars typ = do
    tvs <- mapM (const fresh) pars
    tv <- fresh
    throw (TypeMismatch (typ :-> foldTypes tvs tv) typ)
