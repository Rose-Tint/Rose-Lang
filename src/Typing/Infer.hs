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
    findScoped,

    pushUndefGlobal,
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
    (Except ErrInfo))


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
    -> Either ErrInfo (a, Cons, Table)
runInfer tbl inf = case runExcept wsResult of
    Left err -> Left err
    Right ((a, s), cons) -> Right (a, cons, table s)
    where
        wsResult = runWriterT (runStateT inf (mkState tbl))

throw :: Error -> Infer a
throw err  = do
    pos <- gets position
    let ei = ErrInfo pos (Right err)
    lift (lift (throwE ei))

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

pushNewScoped :: Mutab -> Var -> Type -> Infer Func
pushNewScoped mut name typ = do
    mData <- gets (lookupNewestScope name . table)
    case mData of
        Nothing -> pushScoped mut name typ
        Just dta ->
            let orig = Var (varName name) (getPos dta)
            in throw (Redefinition orig name)

pushScoped :: Mutab -> Var -> Type -> Infer Func
pushScoped mut name typ = do
    pur <- gets purity
    let dta = Func typ Intern pur mut (getPos name)
    modifyEnv (insertScoped name dta)
    return dta

pushUndefGlobal :: Var -> Type -> Infer Func
pushUndefGlobal name typ = do
    pur <- gets purity
    let dta = Func typ Export pur Imut (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushGlobal :: Var -> Visib -> Type -> Infer Func
pushGlobal name vis typ = do
    pur <- gets purity
    let dta = Func typ vis pur Imut (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushData :: Var -> Visib -> Type -> [Var] -> Infer Data
pushData name vis typ ctors = do
    let dta = Data typ vis ctors (getPos name)
    modifyEnv (insertType name dta)
    return dta

searchGlobals :: Var -> Infer Type
searchGlobals name = do
    mData <- gets (lookupGlobal name . table)
    case mData of
        Nothing -> throwUndef name
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
