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
import Middle.Analyzer.SymbolTable
import Middle.SymbolTable


default (Int, Double)


class Checker a where
    infer :: a -> Analyzer Type
    -- |Checks that something is infered to be
    -- compatable with the given type
    isOfType :: a -> Type -> Analyzer Bool
    isOfType a expected = do
        typ <- infer a
        return (typ == expected)


instance Checker Type where
    infer Delayed = peekExpType
    infer t = return t
    isOfType t = return . (t ==)

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
        sdType <$> searchScopeds var
    infer (Application val vals) =
        Applied <$> mapM infer (val:vals)
    infer (CtorCall name args) = do
        updatePosVar name
        typ <- sdType <$> searchGlobals name
        expect typ (apply typ args)
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
    infer (NewVar mut name typ val) = do
        pushScoped mut name typ
        expectCheck typ val
        return NoType
    infer (Reassignment name val) = do
        -- TODO: assert mutability
        dta <- findScoped name
        let typ = sdType dta
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
    infer (FuncDecl pur vis name (TypeDecl _ typ)) = define name $! do
        pushGlobal name $ mkSymbolData
            name typ (Just vis) (Just pur)
        return NoType
    infer (FuncDef name params bdy) = define name $! do
        dta <- searchGlobals name
        let typ = sdType dta
        pushParams params
        bT <- inferBody bdy
        expectCheck typ bT
        return NoType
    infer (DataDef vis name tps ctors) = define name $! do
        pushType name $ mkSymbolData name
            (Type name ((`Param` []) <$> tps))
            (Just vis) (Just Pure)
        mapM_ infer ctors
        return NoType
    infer (TraitDecl vis _ctx name _tps fns) = define name $! do
        pushTrait name $ mkSymbolData
            name Delayed (Just vis) (Just Pure)
        mapM_ infer fns
        return NoType
    infer (TraitImpl _ctx name _types fns) = define name $! do
        _ <- searchTraits name
        mapM_ infer fns
        return NoType
    infer (TypeAlias _vis _alias _typ) = do
        -- pushType alias $ mkSymbolData
        --     alias typ (Just vis) (Just Pure)
        return NoType

instance Checker Ctor where
    infer (SumType name vis types) = define name $! do
        pushGlobal name $ mkSymbolData name
            (Applied types) (Just vis) (Just Pure)
        return NoType
    infer (Record cName vis flds) = define cName $! do
        types <- forM flds $ \(Field fName typ) -> define fName $! do
            pushGlobal fName $ mkSymbolData fName
                (Applied [Delayed, typ])
                (Just vis) (Just Pure)
            return typ
        pushGlobal cName $ mkSymbolData cName
            (Applied (types ++ [Delayed]))
            (Just vis) (Just Pure)
        return NoType

instance Checker Var where
    infer var = sdType <$> findScoped var

instance Checker SymbolData where
    infer = return . sdType


pushParams :: [Value] -> Analyzer Type
pushParams _ = return NoType

apply :: Type -> [Value] -> Analyzer Type
apply ft [] = return ft
apply ft (val:vals) = do
    updatePosVal val
    vT <- infer val
    eT <- peekExpType
    areSame <- isOfType vT eT
    if areSame then
        apply ft vals
    else
        throw $ TypeMismatch vT eT

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
expectCheck t = expectCheck' t

expectCheck' :: Checker a => Type -> a -> Analyzer Type
expectCheck' typ a = do
    aT <- expect typ (infer a)
    if typ == NoType then
        return aT
    else do
        same <- isOfType aT typ
        if same then
            return (typ <:> aT)
        else
            throw $ TypeMismatch typ aT
