module Analysis.Validator (
    validate,
) where

import Prelude hiding (fail)

import Control.Monad (forM, (<$!>))

import Analysis.Analyzer
import Analysis.Error
import Analysis.Table
import AST
import Common.Specifiers
import Common.SrcPos
import Common.Var


default (Int, Double)


class Validator a where
    validate :: a -> Analyzer a


instance Validator Pattern where
    validate val@(Hole p) = do
        updatePos p
        return val
    validate (LitPtrn lit) = LitPtrn <$> validate lit
    validate par@(Param name) = do
        updatePos name
        pushScoped name Imut
        return par
    validate (CtorPtrn name args) = do
        updatePos name
        args' <- mapM validate args
        mData <- lookupGlobal name
        case mData of
            Nothing -> do
                pushUndefCtor name
                return ()
            Just Constructor{} -> return ()
            Just dta -> throw $ Redefinition
                (name{_varPos=getPos dta}) name
        return (CtorPtrn name args')
    validate (TuplePtrn tup) =
        TuplePtrn <$> mapM validate tup
    validate (p1 `OrPtrn` p2) = do
        p1' <- validate p1
        p2' <- validate p2
        return (p1' `OrPtrn` p2')

instance Validator Literal where
    validate val@(IntLit _ p) = do
        updatePos p
        return val
    validate val@(FloatLit _ p) = do
        updatePos p
        return val
    validate val@(DoubleLit _ p) = do
        updatePos p
        return val
    validate val@(CharLit _ p) = do
        updatePos p
        return val
    validate val@(StringLit _ p) = do
        updatePos p
        return val

instance Validator Value where
    validate (Literal lit) = Literal <$> validate lit
    validate val@(VarVal var) = do
        updatePos var
        mData <- lookupScoped var
        case mData of
            Nothing -> do
                pushUndefFunc var
                return ()
            Just _ -> return ()
        return val
    validate (Application v1 v2) = do
        v1' <- validate v1
        v2' <- validate v2
        return (Application v1' v2')
    validate (CtorCall name) = do
        updatePos name
        mData <- lookupGlobal name
        case mData of
            Nothing -> do
                pushUndefCtor name
                return ()
            Just Constructor{} -> return ()
            Just dta -> throw $ Redefinition
                (name{_varPos=getPos dta}) name
        return (CtorCall name)
    validate (Tuple arr) =
        Tuple <$!> mapM validate arr
    validate (Array arr) =
        Array <$!> mapM validate arr
    validate (Lambda params bdy) = do
        -- params' <- mapM validate params
        bdy' <- validate bdy
        return (Lambda params bdy')
    validate (IfElseVal cond tr fa) = do
        cond' <- validate cond
        tr' <- validate tr
        fa' <- validate fa
        return (IfElseVal cond' tr' fa')
    validate (MatchVal val cases) = do
        val' <- validate val
        cases' <- forM cases $ \(p, v) -> do
            p' <- validate p
            v' <- validate v
            return (p', v')
        return (MatchVal val' cases')

-- TODO: check if a return is required
--   (In other words, if there is no return
--   statement anywhere in the current branch,
--   one is required in both bodies)
instance Validator Stmt where
    validate (IfElse cls tb fb) = do
        cls' <- validate cls
        tb' <- validateBody tb
        fb' <- validateBody fb
        return (IfElse cls' tb' fb')
    validate (Loop init' cond iter bdy) = inNewScope $! do
        inNewScope $! do
            init'' <- validate init'
            cond' <- validate cond
            iter' <- validate iter
            bdy' <- inNewScope (validateBody bdy)
            return (Loop init'' cond' iter' bdy')
    validate (Match val cases) = do
        val' <- validate val
        cases' <- forM cases $ \(Case ptrn bdy) -> do
            ptrn' <- validate ptrn
            bdy' <- validateBody bdy
            return (Case ptrn' bdy')
            -- TODO: assert that all 'returned'
            --     vals are of the same type
        return (Match val' cases')
    validate stmt@(NewVar mut name _ _) = do
        pushScoped name mut
        return stmt
    validate stmt@(Reassignment name _val) = do
        _ <- findScoped name
        return stmt
    validate (Return val) = do
        val' <- validate val
        return (Return val')
    validate (ValStmt val) = do
        val' <- validate val
        return (ValStmt val')
    validate Break = do
        allowed <- stAllowBreak <$> getState
        if allowed then return Break else
            fail "illegal `break`"
    validate Continue = do
        allowed <- stAllowBreak <$> getState
        if allowed then return Continue else
            fail "illegal `continue`"
    validate NullStmt = return NullStmt
    validate (Compound bdy) =
        Compound <$> inNewScope (mapM validate bdy)

instance Validator Expr where
    validate expr@(FuncDecl pur vis name _typ) = define name $! do
        pushFunction' name vis pur
        return expr
    validate (FuncDef name params body) = define name $! do
        mData <- lookupGlobal name
        _dta <- case mData of
            Nothing -> pushUndefFunc name
            Just dta -> return dta
        params' <- mapM validate params
        body' <- validateBody body
        return (FuncDef name params' body')
    validate expr@(DataDef vis name _tps ctors) = define name $! do
        pushDatatype name vis
        mapM_ validate ctors
        return expr
    validate expr@(TraitDecl vis _ctx name _tps fns) = define name $! do
        pushTrait name vis
        mapM_ validate fns
        return expr
    -- TODO:
    validate (TraitImpl ctx name types fns) = define name $! do
        mData <- lookupTrait name
        case mData of
            Nothing -> do
                pushUndefTrait name
                return ()
            Just _ -> return ()
        fns' <- mapM validate fns
        return (TraitImpl ctx name types fns')
    validate expr@(TypeAlias _vis _alias _typ) = do
        -- TODO
        return expr

instance Validator Ctor where
    validate ctor@(SumType name vis _types) = do
        parent <- getCurrDef
        define name $! pushCtor name vis parent
        return ctor
    validate (Record _name _vis _flds) = fail
        "Validator.validate: `Record`s are not yet implemented"


validateBody :: Stmt -> Analyzer Stmt
validateBody body = inNewScope (validate body)
