{-# LANGUAGE LambdaCase #-}

module Typing.Inferable (
    makeInference,
    infer,
    inferTopLevel,
) where

import Prelude hiding (lookup)

import Control.Monad (foldM, forM, unless)
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


inferTopLevel :: [Expr] -> Either Error Table
inferTopLevel [] = Right emptyTable
inferTopLevel exprs = case runInfer emptyTable inf of
    Left err -> Left err
    Right ((), _cons, tbl) -> Right tbl
    where
        inf = mapM_ inferTop exprs

makeInference :: Table -> Infer Type -> Either Error Scheme
makeInference tbl inf = case runInfer tbl inf of
    Left err -> Left err
    Right (typ, cons, _tbl) -> runSolver typ cons


class Inferable a where
    infer :: a -> Infer Type


instance Inferable Type where
    infer (Type name types) =
        Type name <$> mapM infer types
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
    infer (IntLit _ p) = do
        updatePos p
        return intType
    infer (FloatLit _ p) = do
        updatePos p
        return floatType
    infer (DoubleLit _ p) = do
        updatePos p
        return doubleType
    infer (CharLit _ p) = do
        updatePos p
        return charType
    infer (StringLit _ p) = do
        updatePos p
        return stringType

instance Inferable Value where
    infer (Literal lit) = infer lit
    {- ^
    x : σ ∈ Γ
    -----------------------
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
    {- ^
    Γ, (x1 : τ1, ..., xn : τn) ⊢ e : τ(n+1)
    ----------------------------
    Γ ⊢ λ(x1 ... xn) => e : (τ1 -> ... -> τ(n+1))
    -}
    infer (Lambda ps body) = inNewScope $ do
        unless (null ps) (updatePos (head ps))
        pTs <- mapM (const fresh) ps
        tv <- fresh
        let typ = foldTypes pTs tv
        aftParsT <- applyParams (Param <$> ps) typ
        bT <- infer body
        constrain tv typ
        return (aftParsT :-> bT)
    {- ^
    Γ ⊢ e1 : τ1    ...    Γ ⊢ en : τn
    ---------------------------------
    Γ ⊢ (e1, ..., en) : (τ1, ..., τn)
    -}
    infer (Tuple arr) = tupleOf <$> mapM infer (A.elems arr)
    {- ^
    Γ ⊢ e1 : τ1    ...    Γ ⊢ en : τ1
    ------------------------------
    Γ ⊢ [e1, ..., en] : [τ1]
    -}
    infer (Array arr) = do
        tv <- fresh
        arrayOf <$> foldM (\ t1 val -> do
            t2 <- infer val
            constrain t1 t2
            return t2
            ) tv arr
    {- ^
    Γ ⊢ e1 : Bool    Γ ⊢ e2 : τ1    Γ ⊢ e3 : τ1
    -------------------------------------------
    Γ ⊢ if e1 then e2 else e3 : e3
    -}
    infer (IfElseVal cond tr fa) = do
        cT <- infer cond
        tT <- infer tr
        fT <- infer fa
        constrain cT boolType
        constrain tT fT
        return tT
    {- ^
    Γ ⊢ e1 : τ1    (Γ ⊢ e2 : τ1    Γ ⊢ e3 : τ2
    ...    Γ ⊢ en : τ1    Γ ⊢ e(++n) : τ2)
    -----------------------------------------------
    Γ ⊢ match e1 { (e2 => e3 ... en => e(n+1)) } : τ2
    -}
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
    {- ^
    ------------
    Γ ⊢ x : τ
    -}
    infer (Param name) = do
        updatePos name
        func <- pushParam name
        return $! funcType func
    {- ^
    ------------
    Γ ⊢ _ : τ
    -}
    infer (Hole p) = do
        updatePos p
        fresh
    {- ^
    x: σ ∈ Γ ... ???
    ------------
    Γ ⊢ [x e1 ... e2] : σ
    -}
    infer (CtorPtrn name args) = do
        updatePos name
        t1 <- searchGlobals name
        t2 <- applyParams args t1
        -- tv <- fresh
        -- constrain t1 (t2 :-> tv)
        -- return (t2 :-> t1)
        return t2
    {- ^
    Γ ⊢ e1 : τ1  ...  Γ ⊢ en : τn
    ---------------------------------------
    Γ ⊢ [(e1, ..., e2)] : (τ1, ..., τn)
    -}
    infer (TuplePtrn args) = tupleOf <$> mapM infer args
    infer (LitPtrn lit) = infer lit
    {- ^
    Γ ⊢ e1 : τ1  ...  Γ ⊢ en : τ1
    -----------------------------------
    Γ ⊢ [e1, en] : τ1
    -}
    infer (OrPtrn p1 p2) = do
        t1 <- infer p1
        t2 <- infer p2
        constrain t1 t2
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
