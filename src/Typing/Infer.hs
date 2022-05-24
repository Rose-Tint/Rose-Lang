{-# LANGUAGE Rank2Types #-}

module Typing.Infer (
    Inference(..),
    AnalyzerT,
    Analyzer,
    Infer,
    runAnalyzer,
    runInfer,
    fresh,

    throw,
    -- recover,
    throwUndef,

    searchGlobals,
    searchScopeds,
    findScoped,

    pushGlobal,
    pushScoped,
    pushNewScoped,
    pushData,

    gets,
    updatePos,
    allowJumpsIn,
    jumpAllowed,
    setPurityIn,
    purity,
    inNewScope,

    instantiate,
    generalize,
    constrain,
) where

import Prelude hiding (lookup)

import Control.Monad (replicateM)
import Control.Monad.Trans.Class
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


data AnState = AnState {
    jumpAllowed :: Bool,
    purity :: Purity,
    table :: Table,
    position :: SrcPos,
    freshIdx :: Int
    }

type Cons = [(Type, Type)]

type AnalyzerT = StateT AnState
type Analyzer = State AnState

type Infer = AnalyzerT
    (WriterT Cons
    (Writer [ErrInfo]))

data Inference a = Inf !a Cons Table [ErrInfo]


mkState :: Table -> AnState
mkState tbl = AnState {
    jumpAllowed = False,
    purity = Pure,
    table = tbl,
    position = newSrcPos,
    freshIdx = 0
    }

runAnalyzer :: Analyzer a -> (a, Table)
runAnalyzer an =
    let (a, s) = runState an (mkState emptyTable)
    in (a, table s)

runInfer :: Table -> Infer a -> Inference a
runInfer tbl inf =
    let (((a, s), cons), errs) = runWriter wsResult
    in Inf a cons (table s) errs
    where
        wsResult = runWriterT (runStateT inf (mkState tbl))

throw :: Error -> Infer ()
throw err  = do
    pos <- gets position
    let ei = ErrInfo pos (Right err)
    lift (lift (tell [ei]))

throwUndef :: Var -> Infer ()
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

setPurityIn :: Purity -> Infer a -> Infer a
setPurityIn pur m = do
    prev <- gets purity
    modify (\s -> s { purity = pur })
    x <- m
    modify (\s -> s { purity = prev })
    return $! x

allowJumpsIn :: Infer a -> Infer a
allowJumpsIn m = do
    prev <- gets jumpAllowed
    modify $ \s -> s { jumpAllowed = True }
    x <- m
    modify $ \s -> s { jumpAllowed = prev }
    return $! x

pushNewScoped :: Var -> Type -> Infer Func
pushNewScoped name typ = do
    mData <- gets (lookupNewestScope name . table)
    case mData of
        Nothing -> pushScoped name typ
        Just dta -> do
            let orig = Var (varName name) (getPos dta)
            throw (Redefinition orig name)
            return dta

pushScoped :: Var -> Type -> Infer Func
pushScoped name typ = do
    pur <- gets purity
    let dta = Func typ pur (getPos name)
    modifyEnv (insertScoped name dta)
    return dta

pushGlobal :: Var -> Type -> Infer Func
pushGlobal name typ = do
    pur <- gets purity
    let dta = Func typ pur (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushData :: Var -> Type -> [Var] -> Infer Data
pushData name typ ctors = do
    let dta = Data typ ctors (getPos name)
    modifyEnv (insertType name dta)
    return dta

searchGlobals :: Var -> Infer Type
searchGlobals name = do
    mData <- gets (lookupGlobal name . table)
    case mData of
        Nothing -> do
            -- throwUndef name
            tv <- fresh
            pushGlobal name tv
            return tv
        Just dta -> return $! funcType dta

searchScopeds :: Var -> Infer Type
searchScopeds name = do
    mData <- gets (lookupScoped' name . table)
    case mData of
        Nothing -> searchGlobals name
        Just dta -> return $! funcType dta

findScoped :: Var -> Infer Type
findScoped name = do
    mData <- gets (lookupScoped' name . table)
    case mData of
        Nothing -> do
            throwUndef name
            tv <- fresh
            pushGlobal name tv
            return tv
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
