{-# LANGUAGE Rank2Types #-}

module Typing.Infer (
    Infer,
    runInfer,
    fresh,

    throw,
    -- recover,
    throwUndef,

    searchGlobals,
    searchScopeds,

    pushUndefCtor,
    pushUndefGlobal,
    pushParam,
    pushScoped,

    gets,
    modify,
    modifyEnv,
    updatePos,
    allowJumpsIn,
    jumpAllowed,
    purity,

    findScoped,
    inNewScope,
    instantiate,
    generalize,
    constrain,
) where

import Prelude hiding (lookup)

import Control.Monad (replicateM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.Set as S

import Analysis.Error
import Common.Specifiers
import Common.SrcPos
import Common.Var
import Data.Table
import qualified Data.VarMap as M
import Typing.Scheme
import Typing.Substitution
import Typing.Type


data InfState = InfState {
    jumpAllowed :: Bool,
    purity :: Purity,
    table :: Table,
    position :: SrcPos,
    freshIdx :: Int
    }

type Cons = [(Type, Type)]

type Infer =
    StateT InfState
    (WriterT Cons
    (Except Error))


mkState :: Table -> InfState
mkState tbl = InfState {
    jumpAllowed = False,
    purity = Pure,
    table = tbl,
    position = newSrcPos,
    freshIdx = 0
    }

runInfer :: Table -> Infer a
    -> Either Error (a, Cons, Table)
runInfer tbl inf = case runExcept wsResult of
    Left err -> Left err
    Right ((a, s), cons) -> Right (a, cons, table s)
    where
        wsResult = runWriterT (runStateT inf (mkState tbl))

throw :: Error -> Infer a
throw = lift . lift . throwE

throwUndef :: Var -> Infer a
throwUndef name = do
    similars <- gets (getSimilarVars name . table)
    throw (Undefined name similars)

updatePos :: HasSrcPos a => a -> Infer ()
updatePos p = case getPos p of
    UnknownPos -> return ()
    pos -> modify $ \s -> s { position = pos }

freshLetters :: [String]
freshLetters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    i <- gets freshIdx
    pos <- gets position
    let var = Var (freshLetters !! i) pos
    modify $ \s -> s { freshIdx = i + 1 }
    return (TypeVar var)

modifyEnv :: (Table -> Table) -> Infer ()
modifyEnv f = modify $ \s -> s { table = f (table s) }

inNewScope :: Infer a -> Infer a
inNewScope m = do
    modifyEnv addScope
    x <- m
    modifyEnv remScope
    return $! x

allowJumpsIn :: Infer a -> Infer a
allowJumpsIn m = do
    prev <- gets jumpAllowed
    modify $ \s -> s { jumpAllowed = True }
    x <- m
    modify $ \s -> s { jumpAllowed = prev }
    return $! x

pushUndefCtor :: Var -> Type -> Infer Func
pushUndefCtor name typ = do
    let dta = mkCtor name Export typ
    modifyEnv (insertGlobal name dta)
    return dta

pushUndefGlobal :: Var -> Type -> Infer Func
pushUndefGlobal name typ = do
    pur <- gets purity
    let dta = Func typ Export pur Imut (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushScoped :: Mutab -> Var -> Type -> Infer Func
pushScoped mut name typ = do
    pur <- gets purity
    let dta = Func typ Intern pur mut (getPos name)
    modifyEnv (insertScoped name dta)
    return dta

pushParam :: Var -> Type -> Infer Func
pushParam name typ = do
    pur <- gets purity
    let dta = Func typ Intern pur Imut (getPos name)
    modifyEnv (insertScoped name dta)
    return dta

searchGlobals :: Var -> Infer Type
searchGlobals name = do
    mData <- gets (lookupGlobal name . table)
    case mData of
        Nothing -> do
            tv <- fresh
            pushUndefGlobal name tv
            return tv
        Just dta -> return $! funcType dta

searchScopeds :: Var -> Infer Type
searchScopeds name = do
    mData <- gets (lookupScoped name . table)
    case mData of
        Nothing -> do
            tv <- fresh
            pushUndefGlobal name tv
            return tv
        Just dta -> return $! funcType dta

findScoped :: Var -> Infer Type
findScoped name = do
    mData <- gets (lookupScoped' name . table)
    case mData of
        Nothing -> throwUndef name
        Just dta -> return $! funcType dta

instantiate :: Scheme -> Infer Type
instantiate (Forall vars typ) = do
    vars' <- mapM (const fresh) vars
    let sub = M.fromList (zip vars vars')
    return $! apply sub typ

generalize :: Type -> Infer Scheme
generalize typ = do
    env <- gets table
    let vars = S.toList (ftv typ `S.difference` ftv env)
    return (Forall vars typ)

constrain :: Type -> Type -> Infer ()
constrain t1 t2 = lift (tell [(t1, t2)])
