{-# LANGUAGE LambdaCase #-}

module Middle.Analyzer.Validator (
    validate,
) where

import Prelude hiding (fail)

import Control.Monad (forM, (<$!>))

import Common.Typing
import Common.Var
import Front.Parser
import Middle.Analyzer.Error
import Middle.Analyzer.Internal
import Middle.Analyzer.Table
import Middle.Table
import Middle.Typing.Inferable
import Middle.Typing.Scheme


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
                tv <- fresh
                pushUndefFunc var tv
                return ()
            Just _ -> return ()
        return val
    validate (Application val vals) = do
        val' <- validate val
        vals' <- mapM validate vals
        return (Application val' vals')
    validate val@(CtorCall name args) = do
        updatePosVar name
        mData <- lookupGlobal name
        case mData of
            Nothing -> do
                (sub, typ) <- inferNew val
                pushUndefCtor name (apply sub typ)
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
    validate stmt@(IfElse cls tb fb) = do
        inferNew stmt
        cls' <- validate cls
        tb' <- validateBody tb
        fb' <- validateBody fb
        return (IfElse cls' tb' fb')
    validate stmt@(Loop init' cond iter bdy) = inNewScope $! do
        inNewScope $! do
            init'' <- validate init'
            cond' <- validate cond
            iter' <- validate iter
            bdy' <- inNewScope (validateBody bdy)
            inferNew stmt
            return (Loop init'' cond' iter' bdy')
    validate stmt@(Match val cases) = do
        inferNew stmt
        val' <- validate val
        cases' <- forM cases $ \(ptrn, bdy) -> do
            ptrn' <- validate ptrn
            bdy' <- mapM validate bdy
            return (ptrn', bdy')
            -- TODO: assert that all 'returned'
            --     vals are of the same type
        return (Match val' cases')
    validate stmt@(NewVar mut name _ _) = do
        (sub, typ) <- inferNew stmt
        pushScoped name (apply sub typ) mut
        return stmt
    validate stmt@(Reassignment name val) = do
        -- TODO: assert mutability
        dta <- findScoped name
        (_, vT) <- inferNew val
        _ <- unify (scpType dta) vT
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
    validate expr@(FuncDecl pur vis name typ) = define name $! do
        pushFunction' name typ vis pur
        return expr
    validate expr@(FuncDef name params body) = define name $! do
        mData <- lookupGlobal name
        _dta <- case mData of
            Nothing -> do
                tv <- fresh
                pushUndefFunc name tv
            Just dta -> return dta
        pushParams params
        -- let TypeDecl _ typ = glbType dta
        _ <- validateBody body
        -- expectCheck typ bT
        return expr
    validate expr@(DataDef vis name tps ctors) = define name $! do
        pushDatatype name tps vis
        mapM_ validate ctors
        return expr
    validate expr@(TraitDecl vis _ctx name tps fns) = define name $! do
        pushTrait name tps vis
        mapM_ validate fns
        return expr
    -- TODO:
    validate expr@(TraitImpl _ctx name types fns) = define name $! do
        mData <- lookupTrait name
        case mData of
            Nothing -> do
                pushUndefTrait name types
                return ()
            Just _ -> return ()
        mapM_ validate fns
        return expr
    validate expr@(TypeAlias _vis _alias _typ) = do
        -- pushType alias $ mkSymbolData
        --     alias typ (Just vis) (Just Pure)
        return expr

instance Validator Ctor where
    validate ctor@(SumType name vis types) = do
        parent <- getCurrDef'
        define name $! pushCtor name (Applied types) vis parent
        return ctor
    validate (Record _name _vis _flds) = fail
        "Validator.validate: `Record`s are not yet implemented"
        -- types <- forM flds $ \(Field fName typ) -> define fName $! do
        --     pushGlobal fName $ mkSymbolData fName
        --         (Applied [Delayed, typ])
        --         (Just vis) (Just Pure)
        --     return typ
        -- pushCtor name (Applied types) vis
        -- pushGlobal name $ mkSymbolData name
        --     (Applied (types ++ [Delayed]))
        --     (Just vis) (Just Pure)
        -- return NoType


pushParams :: [Value] -> Analyzer ()
pushParams [] = return ()
pushParams (val:vals) = do
    case val of
        VarVal var -> do
            tv <- fresh
            pushScoped var tv Imut
            return ()
        Hole p -> do
            updatePos p
            return ()
        _ -> return ()-- "pushParams: Not yet implemented"
    pushParams vals

-- apply :: [Type] -> [Value] -> Analyzer Type
-- apply fTs [] = return (Applied fTs)
-- apply (t:ts) (val:vals) = do
--     updatePosVal val
--     -- expectCheck t val
--     apply ts vals
-- apply [] vals = do
--     tv <- fresh
--     throw $ TypeMismatch eT (Applied (tv <$ vals))


validateBody :: Body -> Analyzer Body
validateBody body = inNewScope $! do
    -- TODO: check if body has/needs a return
    (_, _) <- inferNew body
    mapM validate body

-- expectCheck :: Validator a => Type -> a -> Analyzer Type
-- expectCheck typ_ a = do
--     let typ = normalize typ_
--     aT_ <- expect typ (validate a)
--     let aT = normalize aT_
--     case typ <::> aT of
--         NoType -> throw $ TypeMismatch typ aT
--         typ' -> return (normalize typ')