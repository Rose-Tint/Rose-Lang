module Analysis.Analyzer () where



type AnalyzerT = StateT AnState

type Analyzer = State AnState


data AnState = AnState {
    state_jumpAllowed :: Bool,
    state_purity :: Purity,
    state_table :: Table,
    state_position :: SrcPos,
    state_freshIdx :: Int,
    state_errors :: [ErrInfo]
    }

mkState :: Table -> AnState
mkState tbl = AnState {
    state_jumpAllowed = False,
    state_purity = Pure,
    state_table = tbl,
    state_position = newSrcPos,
    state_freshIdx = 0,
    state_errors = []
    }

runAnalyzer :: Analyzer a -> (a, Table, [ErrInfo])
runAnalyzer an =
    let (a, s) = runState an (mkState emptyTable)
    in (a, table s, errors s)

updatePos :: Monad m => HasSrcPos a => a -> AnalyzerT m ()
updatePos p = case getPos p of
    UnknownPos -> return ()
    pos -> modify $ \s -> s { state_position = pos }

position :: Monad m => AnalyzerT m SrcPos
position = gets state_position

purity :: Monad m => AnalyzerT m Purity
purity = gets state_purity

isJumpAllowed :: Monad m => AnalyzerT m Bool
isJumpAllowed = gets state_jumpAllowed

freshLetters :: [String]
freshLetters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Monad m => AnalyzerT m Type
fresh = do
    i <- gets freshIdx
    pos <- position
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
    prev <- purity
    modify (\s -> s { purity = pur })
    x <- m
    modify (\s -> s { purity = prev })
    return $! x

allowJumpsIn :: Monad m => AnalyzerT m a -> AnalyzerT m a
allowJumpsIn m = do
    prev <- isJumpAllowed
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
    pur <- purity
    let dta = Func typ Export pur Imut (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushGlobal :: Monad m => Var -> Visib -> Type -> AnalyzerT m Func
pushGlobal name vis typ = do
    pur <- purity
    let dta = Func typ vis pur Imut (getPos name)
    modifyEnv (insertGlobal name dta)
    return dta

pushScoped :: Monad m => Mutab -> Var -> Type -> AnalyzerT m Func
pushScoped mut name typ = do
    pur <- purity
    let dta = Func typ Intern pur mut (getPos name)
    modifyEnv (insertScoped name dta)
    return dta

pushParam :: Monad m => Var -> AnalyzerT m Func
pushParam name = do
    pur <- purity
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

findScoped :: Monad m => Var -> AnalyzerT m Type
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

{- %%%%% ERRORS %%%%% -}

throw :: Monad m => Error -> AnalyzerT m a
throw err  = do
    pos <- gets position
    let ei = ErrInfo pos (Right err)
    modify (\s -> s { state_errors = (ei:state_errors s) })

typeMismatch :: Monad m => Type -> Type -> AnalyzerT m a
typeMismatch = throw . TypeMismatch

redefinition :: Monad m => Var -> Var -> AnalyzerT m a
redefinition = throw . Redefinition

infiniteType :: Monad m => Var -> Type -> AnalyzerT m a
infiniteType = throw . InfiniteType

illegalBreak :: Monad m => AnalyzerT m a
illegalBreak = throw (OtherError "illegal 'break'")

illegalContinue :: Monad m => AnalyzerT m a
illegalContinue = throw (OtherError "illegal 'continue'")

missingReturn :: Monad m => Var -> AnalyzerT m a
missingReturn = throw . MissingReturn

otherError :: Monad m => String -> AnalyzerT m a
missingReturn = throw . OtherError

throwUndef :: Var -> Infer a
throwUndef name = do
    similars <- gets (getSimilarVars name . table)
    throw (Undefined name similars)
