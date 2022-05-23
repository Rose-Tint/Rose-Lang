{-# LANGUAGE Rank2Types #-}

module Typing.Infer (
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

    pushUndefCtor,
    pushUndefGlobal,
    pushGlobal,
    pushParam,
    pushScoped,
    pushData,

    gets,
    updatePos,
    allowJumpsIn,
    jumpAllowed,
    setPurityIn,
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
    (Except Error))


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

updatePos :: Monad m => HasSrcPos a => a -> AnalyzerT m ()
updatePos p = case getPos p of
    UnknownPos -> return ()
    pos -> modify $ \s -> s { position = pos }

freshLetters :: [String]
freshLetters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Monad m => AnalyzerT m Type
fresh = do
    i <- gets freshIdx
    pos <- gets position
    let var = Var (freshLetters !! i) pos
    modify $ \s -> s { freshIdx = i + 1 }
    return (TypeVar var)

modifyEnv :: Monad m => (Table -> Table) -> AnalyzerT m ()
modifyEnv f = modify $ \s -> s { table = f (table s) }

inNewScope :: Monad m => AnalyzerT m a -> AnalyzerT m a
inNewScope m = do
    modifyEnv addScope
    x <- m
    modifyEnv remScope
    return $! x

setPurityIn :: Monad m => Purity -> AnalyzerT m a -> AnalyzerT m a
setPurityIn pur m = do
    prev <- gets purity
    modify (\s -> s { purity = pur })
    x <- m
    modify (\s -> s { purity = prev })
    return $! x

allowJumpsIn :: Monad m => AnalyzerT m a -> AnalyzerT m a
allowJumpsIn m = do
    prev <- gets jumpAllowed
    modify $ \s -> s { jumpAllowed = True }
    x <- m
    modify $ \s -> s { jumpAllowed = prev }
    return $! x

pushUndefCtor :: Monad m => Var -> Type -> AnalyzerT m Func
pushUndefCtor name typ = do
    let dta = mkCtor name Export typ
    modifyEnv (insertGlobal name dta)
    return dta

pushUndefGlobal :: Monad m => Var -> Type -> AnalyzerT m Func
pushUndefGlobal name typ = do
    pur <- gets purity
    let dta = Func typ Export pur Imut (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushGlobal :: Monad m => Var -> Visib -> Type -> AnalyzerT m Func
pushGlobal name vis typ = do
    pur <- gets purity
    let dta = Func typ vis pur Imut (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushScoped :: Monad m => Mutab -> Var -> Type -> AnalyzerT m Func
pushScoped mut name typ = do
    pur <- gets purity
    let dta = Func typ Intern pur mut (getPos name)
    modifyEnv (insertScoped name dta)
    return dta

pushParam :: Monad m => Var -> AnalyzerT m Func
pushParam name = do
    pur <- gets purity
    typ <- fresh
    let dta = Func typ Intern pur Imut (getPos name)
    modifyEnv (insertScoped name dta)
    return dta

pushData :: Monad m => Var -> Visib -> Type -> [Var] -> AnalyzerT m Data
pushData name vis typ ctors = do
    let dta = Data typ vis ctors (getPos name)
    modifyEnv (insertType name dta)
    return dta

searchGlobals :: Monad m => Var -> AnalyzerT m Type
searchGlobals name = do
    mData <- gets (lookupGlobal name . table)
    case mData of
        Nothing -> do
            tv <- fresh
            pushUndefGlobal name tv
            return tv
        Just dta -> return $! funcType dta

searchScopeds :: Monad m => Var -> AnalyzerT m Type
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

instantiate :: Monad m => Scheme -> AnalyzerT m Type
instantiate (Forall vars typ) = do
    vars' <- mapM (const fresh) vars
    let sub = M.fromList (zip vars vars')
    return $! apply sub typ

generalize :: Monad m => Type -> AnalyzerT m Scheme
generalize typ = do
    env <- gets table
    let vars = S.toList (ftv typ `S.difference` ftv env)
    return (Forall vars typ)

constrain :: Type -> Type -> Infer ()
constrain t1 t2 = lift (tell [(t1, t2)])
