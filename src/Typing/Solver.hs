module Typing.Solver (
    runSolver,
) where

import Control.Monad.Trans.Except
import qualified Data.Set as S

import Analysis.Error
import Common.SrcPos
import Common.Var
import qualified Data.VarMap as M
import Typing.Type
import Typing.Scheme
import Typing.Substitution

-- import Debug.Trace
-- import Text.Pretty


-- Constraint: `(t1, t2)` states that an occurrence
-- of `t1` returns a type of `t2` (i think???)
type Cons = [(Type, Type)]

type Unifier = (Subst, Cons)

type Solver a = Except Error a


runSolver :: Type -> Cons -> Either ErrInfo Scheme
runSolver typ cons =
    case runExcept (solver (nullSubst, cons)) of
        Left err -> Left (ErrInfo (err <?> typ) (Right err))
        Right sub ->
            let typ' = apply sub typ
                vars = S.toList (ftv typ' <> ftv sub)
            in Right (Forall vars typ')

-- |Unifies two types according to the following rules.
-- (is left preferential (assumes that the left arg is
-- the expected one.))
--
-- - if lt or rt is a type-var (that does not occur in
--     the other), then bind
-- - if both are 'applied' and are of the same length,
--     then unify each 'sub'-type
-- - otherwise, fail
unify :: Type -> Type -> Solver Subst
unify (TypeVar nm) typ = bind nm typ
unify typ (TypeVar nm) = bind nm typ
unify t1@(Type nm1 ts1) t2@(Type nm2 ts2)
    | nm1 /= nm2 = throwE (TypeMismatch t1 t2)
    | length ts1 == length ts2 = unifyMany ts1 ts2
unify (l1 :-> l2) (r1 :-> r2) = unifyMany [l1, l2] [r1, r2]
unify (ArrayType t1) (ArrayType t2) = unify t1 t2
unify (TupleType ts1) (TupleType ts2) = unifyMany ts1 ts2
unify t1 t2 = throwE (TypeMismatch t1 t2)

unifyMany :: [Type] -> [Type] -> Solver Subst
unifyMany [] [] = return nullSubst
unifyMany (t1:ts1) (t2:ts2) = do
    s1 <- unify t1 t2
    s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
    return $! s2 <|> s1
unifyMany _ _ = throwE $ OtherError
    "Unification Error"

-- | Binds a variable to a type in a substitution.
-- Also performs an occurence check.
bind :: Var -> Type -> Solver Subst
bind nm typ
    | typ == TypeVar nm = return nullSubst
    | nm ~> typ = throwE (InfiniteType nm typ)
    | otherwise = return $! M.singleton nm typ

-- | Composes constraints and applies their solution,
-- eventually converging on the 'most general rule',
-- which, when applied over a signature, yields its
-- `principal type solution`
solver :: Unifier -> Solver Subst
solver (s1, cs1) = case cs1 of
    [] -> return s1
    ((t1, t2):cs) -> do
        -- let !_ = traceId ("....... t1: "+|t1)
        -- let !_ = traceId ("....... t2: "+|t2)
        s2 <- unify t1 t2
        -- let !_ = traceId ("....... s2: "+|concatMap
        --         (\(k, v) -> "\n"+|k|+": "+|v) (M.assocs (s2 <|> s1)))
        let s3 = s2 <|> s1
        solver (s3, apply s3 cs)
