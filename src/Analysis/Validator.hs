{-# LANGUAGE LambdaCase #-}

module Analysis.Validator (
    validate,
) where

import Prelude hiding (fail)

import Control.Monad (forM, (<$!>))
import qualified Data.Array as A (elems)

import Common.Var
import Front.Parser
import Middle.Analyzer.Error
import Middle.Analyzer.Internal
import Middle.Analyzer.Table
import Middle.Table


default (Int, Double)


class Validator a where
    validate :: a -> Analyzer a


instance Validator Value where
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
    validate val@(VarVal var) = do
        updatePosVar var
        mData <- lookupScoped var
        case mData of
            Nothing -> do
                pushUndefFunc var
                return ()
            Just _ -> return ()
        return val
    validate (Application val vals) = do
        val' <- validate val
        vals' <- mapM validate vals
        return (Application val' vals')
    validate (CtorCall name args) = do
        updatePosVar name
        mData <- lookupGlobal name
        case mData of
            Nothing -> do
                pushUndefCtor name
                return ()
            Just Constructor{} -> return ()
            Just dta -> throw $ Redefinition (name{varPos=glbPos dta}) name
        args' <- mapM validate args
        return (CtorCall name args')
    validate (Tuple arr) =
        Tuple <$!> mapM validate arr
    validate (Array arr) =
        -- TODO: verify that they are all of the same type
        Array <$!> mapM validate arr
    validate (Lambda params bdy) = do
        -- params' <- mapM validate params
        bdy' <- mapM validate bdy
        return (Lambda params bdy')
    validate (StmtVal stmt) = do
        stmt' <- validate stmt
        return (StmtVal stmt')
    validate val@(Hole p) = updatePos p >> return val

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
        cases' <- forM cases $ \(ptrn, bdy) -> do
            ptrn' <- validate ptrn
            bdy' <- mapM validate bdy
            return (ptrn', bdy')
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

instance Validator Expr where
    validate expr@(FuncDecl pur vis name _typ) = define name $! do
        pushFunction' name vis pur
        return expr
    validate expr@(FuncDef name params body) = define name $! do
        mData <- lookupGlobal name
        _dta <- case mData of
            Nothing -> pushUndefFunc name
            Just dta -> return dta
        pushParams params
        _ <- validateBody body
        return expr
    validate expr@(DataDef vis name _tps ctors) = define name $! do
        pushDatatype name vis
        mapM_ validate ctors
        return expr
    validate expr@(TraitDecl vis _ctx name _tps fns) = define name $! do
        pushTrait name vis
        mapM_ validate fns
        return expr
    -- TODO:
    validate expr@(TraitImpl _ctx name _types fns) = define name $! do
        mData <- lookupTrait name
        case mData of
            Nothing -> do
                pushUndefTrait name
                return ()
            Just _ -> return ()
        mapM_ validate fns
        return expr
    validate expr@(TypeAlias _vis _alias _typ) = do
        -- TODO
        return expr

instance Validator Ctor where
    validate ctor@(SumType name vis _types) = do
        parent <- getCurrDef'
        define name $! pushCtor name vis parent
        return ctor
    validate (Record _name _vis _flds) = fail
        "Validator.validate: `Record`s are not yet implemented"


pushParams :: [Value] -> Analyzer ()
pushParams [] = return ()
pushParams (val:vals) = do
    case val of
        IntLit _ p -> updatePos p
        FloatLit _ p -> updatePos p
        DoubleLit _ p -> updatePos p
        CharLit _ p -> updatePos p
        StringLit _ p -> updatePos p
        VarVal var -> do
            pushScoped var Imut
            return ()
        CtorCall name pars -> do
            updatePosVar name
            pushParams pars
        Hole p -> updatePos p
        Tuple pars  -> pushParams (A.elems pars)
        _ -> fail "pushParams: Not yet implemented"
    pushParams vals

validateBody :: Body -> Analyzer Body
validateBody body = inNewScope $! do
    -- TODO: check if body has/needs a return
    mapM validate body
