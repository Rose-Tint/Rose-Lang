{-# LANGUAGE LambdaCase #-}

module Middle.Analyzer.Checker (
    infer,
) where

import Prelude hiding (fail)

import Control.Monad (foldM, forM)
import Data.Array (elems)

import Common.Typing
import Common.Var
import Front.Parser
import Middle.Analyzer.Error
import Middle.Analyzer.Internal
import Middle.Analyzer.Table
import Middle.Table


default (Int, Double)


class Checker a where
    infer :: a -> Analyzer Type
    -- |Checks that something is infered to be
    -- compatable with the given type
    -- isOfType :: a -> Type -> Analyzer Bool
    -- isOfType a ex = do
    --     typ <- infer a
    --     return ((typ <::> ex) == NoType)


instance Checker Type where
    infer Delayed = peekExpType
    infer t = return t

instance Checker Value where
    infer (IntLit _ p) = updatePos p >>
        return intType
    infer (FloatLit _ p) = updatePos p >>
        return floatType
    infer (DoubleLit _ p) = updatePos p >>
        return doubleType
    infer (CharLit _ p) = updatePos p >>
        return charType
    infer (StringLit _ p) = updatePos p >>
        return stringType
    infer (VarVal var) = do
        updatePosVar var
        -- TODO: add if not found
        mData <- lookupScoped var
        case mData of
            Nothing -> do
                eT <- peekExpType
                dta <- pushUndefFunc var eT
                -- TODO: dont ignore constraints
                let TypeDecl _ typ = glbType dta
                return typ
            Just dta ->
                -- TODO: dont ignore constraints
                let TypeDecl _ typ = scpType dta
                in return typ
    infer (Application val vals) =
        Applied <$> mapM infer (val:vals)
    infer (CtorCall name args) = do
        updatePosVar name
        mData <- lookupGlobal name
        case mData of
            Nothing -> do
                typ <- expect Delayed (apply (Delayed <$ args) args)
                dta <- pushUndefCtor name typ
                let TypeDecl _ typ' = glbType dta
                return typ'
            Just (Constructor (TypeDecl _ typ@(Applied types)) _ _ _) ->
                expect typ (apply types args)
            Just (Constructor (TypeDecl _ typ) _ _ _) -> return typ
            Just dta -> throw $ Redefinition (name{varPos=glbPos dta}) name
        -- TODO: Verify that `findGlobals` returns a `Constructor`
    infer (Tuple arr) = do
        typs <- elems <$> mapM infer arr
        return $! tupleOf typs
    infer (Array arr) = do
        eT <- peekExpType
        typ <- foldM (\bT val -> do
            vT <- infer val
            case bT <:> vT of
                NoType -> throw $ TypeMismatch bT vT
                typ -> return typ
            ) eT arr
        return $! arrayOf typ
    infer (Lambda params bdy) = do
        updatePosVar (head params)
        let pTs = (Delayed:(Delayed <$ params))
            apT = Applied pTs
        bT <- expect apT (inferBody bdy)
        return (Applied (tail pTs ++ [bT]))
    infer (StmtVal stmt) = infer stmt
    infer (Hole p) = updatePos p >> peekExpType

instance Checker Stmt where
    infer (IfElse cls trueBody falseBody) = do
        expectCheck' boolType cls
        tT <- inferBody trueBody
        fT <- inferBody falseBody
        return (tT <::> fT)
    infer (Loop _ cond _ bdy) = do
        expectCheck' boolType cond
        inferBody bdy
    infer (Match val cases) = do
        vT <- infer val
        bTs <- forM cases $ \(ptrn, bdy) -> do
            expectCheck vT ptrn
            -- TODO: assert that all 'returned'
            --     vals are of the same type
            inferBody bdy
        return (foldr (<::>) Delayed bTs)
    infer (NewVar mut name td@(TypeDecl _ typ) val) = do
        pushScoped name td mut
        expectCheck typ val
        return NoType
    infer (Reassignment name val) = do
        -- TODO: assert mutability
        dta <- findScoped name
        let TypeDecl _ typ = scpType dta
        expectCheck' typ val
    infer (Return val) = do
        updatePosVal val
        eT <- peekExpType
        case eT of
            -- NoType -> {- set return-type if Delayed -}
            _ -> expectCheck eT val
    infer (ValStmt val) = do
        -- TODO: assert purity
        infer val
    -- TODO: add ability to check if we are in a loop
    --     instead of throwing
    infer Break = throw $ OtherError "illegal `break`"
    infer Continue = throw $ OtherError "illegal `continue`"
    infer NullStmt = return NoType

instance Checker Expr where
    infer (FuncDecl pur vis name typ) = define name $! do
        pushFunction' name typ vis pur
        return NoType
    infer (FuncDef name params bdy) = define name $! do
        mData <- lookupGlobal name
        dta <- case mData of
            Nothing -> pushUndefFunc name Delayed
            Just dta -> return dta
        pushParams params
        let TypeDecl _ typ = glbType dta
        bT <- inferBody bdy
        expectCheck typ bT
        return NoType
    infer (DataDef vis name tps ctors) = define name $! do
        pushDatatype name tps vis
        mapM_ infer ctors
        return NoType
    infer (TraitDecl vis _ctx name tps fns) = define name $! do
        pushTrait name tps vis
        mapM_ infer fns
        return NoType
    -- TODO:
    infer (TraitImpl _ctx name types fns) = define name $! do
        mData <- lookupTrait name
        case mData of
            Nothing -> do
                pushUndefTrait name types
                return ()
            Just _ -> return ()
        mapM_ infer fns
        return NoType
    infer (TypeAlias _vis _alias _typ) = do
        -- pushType alias $ mkSymbolData
        --     alias typ (Just vis) (Just Pure)
        return NoType

instance Checker Ctor where
    infer (SumType name vis types) = do
        parent <- getCurrDef'
        define name $! pushCtor name (Applied types) vis parent
        return NoType
    infer (Record _name _vis _flds) = fail
        "Checker.infer: `Record`s are not yet implemented"
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


pushParams :: [Value] -> Analyzer Type
pushParams [] = return NoType
pushParams (val:vals) = do
    case val of
        VarVal var -> do
            pushScoped var (TypeDecl [] Delayed) Imut
            return ()
        Hole p -> do
            updatePos p
            return ()
        _ -> return ()-- "pushParams: Not yet implemented"
    pushParams vals

apply :: [Type] -> [Value] -> Analyzer Type
apply fTs [] = return (Applied fTs)
apply (t:ts) (val:vals) = do
    updatePosVal val
    expectCheck t val
    apply ts vals
apply [] vals = do
    eT <- peekExpType
    throw $ TypeMismatch eT (Applied (Delayed <$ vals))


inferBody :: Body -> Analyzer Type
inferBody body = do
    eT <- peekExpType
    foldM expectCheck eT body

-- inferBody' :: Body -> Analyzer Type
-- inferBody' body = do
--     eT <- peekExpType
--     foldM expectCheck' eT body

-- expectCheck_ :: Checker a => Type -> a -> Analyzer ()
-- expectCheck_ typ a = do
--     aT <- expect typ (infer a)
--     isOfType typ aT
--     return ()

expectCheck :: Checker a => Type -> a -> Analyzer Type
expectCheck NoType = infer
expectCheck Delayed = infer
expectCheck t = expectCheck' t

expectCheck' :: Checker a => Type -> a -> Analyzer Type
expectCheck' typ_ a = do
    let typ = normalize typ_
    aT_ <- expect typ (infer a)
    let aT = normalize aT_
    case typ <::> aT of
        NoType -> throw $ TypeMismatch typ aT
        typ' -> return (normalize typ')
