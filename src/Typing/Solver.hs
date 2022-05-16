module Typing.Solver (
    runSolver
) where

import Control.Monad.Trans.Except
import qualified Data.Set as S

import Analysis.Error
import Common.Var
import Data.VarMap (singleton)
import Typing.Type
import Typing.Scheme
import Typing.Substitution


-- Constraint: `(t1, t2)` states that an occurrence
-- of `t1` returns a type of `t2` (i think???)
type Cons = [(Type, Type)]

type Unifier = (Subst, Cons)

type Solver a = Except Error a


runSolver :: Type -> Cons -> Either Error Scheme
runSolver typ cons =
    case runExcept (solver (nullSubst, cons)) of
        Left err -> Left err
        Right sub ->
            let typ' = apply sub typ
                vars = S.toList (ftv typ')
            in Right (Forall vars typ')

emptyUni :: Unifier
emptyUni = (nullSubst, [])

-- |Unifies two types according to the following rules.
-- (is left preferential (assumes that the left arg is
-- the expected one.))
--
-- - if lt or rt is a type-var (that does not occur in the
--     other), then bind
-- - if both are 'applied' and are of the same length, then
--     unify each 'sub'-type
-- - otherwise, fail
unify :: Type -> Type -> Solver Unifier
unify (TypeVar nm) typ = bind nm typ
unify typ (TypeVar nm) = bind nm typ
unify t1@(Type nm1 ts1) t2@(Type nm2 ts2)
    | nm1 /= nm2 = throwE (TypeMismatch t1 t2)
    | length ts1 == length ts2 = unifyMany ts1 ts2
unify (l1 :-> l2) (r1 :-> r2) = do
    (s1, cs1) <- unify l1 r1
    (s2, cs2) <- unify (apply s1 l2) (apply s1 r2)
    return (s2 <|> s1, cs1 ++ cs2)
unify (ArrayType t1) (ArrayType t2) = unify t1 t2
unify (TupleType ts1) (TupleType ts2) =
    unifyMany ts1 ts2
unify t1 t2 = throwE (TypeMismatch t1 t2)

unifyMany :: [Type] -> [Type] -> Solver Unifier
unifyMany [] [] = return emptyUni
unifyMany (t1:ts1) (t2:ts2) = do
    (s1, cs1) <- unify t1 t2
    (s2, cs2) <- unifyMany ts1 ts2
    return (s1 <|> s2, cs1 ++ cs2)
unifyMany _ _ = throwE (OtherError "Unification Mismatch")

-- | Binds a variable to a type in a substitution.
-- Also performs an occurence check.
bind :: Var -> Type -> Solver Unifier
bind _ (TypeVar _) = return emptyUni
bind nm typ
    | nm ~> typ = throwE (InfiniteType nm typ)
    | otherwise = return (singleton nm typ, [])

-- | Composes constraints and applies their solution,
-- eventually converging on the 'most general rule',
-- which, when applied over a signature, yields its
-- `principal type solution`
solver :: Unifier -> Solver Subst
solver (s1, cs1) = case cs1 of
    [] -> return s1
    ((t1, t2):cs2) -> do
        (s2, cs3) <- unify t1 t2
        let cons = cs3 ++ fmap (\(t1', t2') ->
                (apply s2 t1', apply s2 t2')
                ) cs2
        solver (s2 <|> s1, cons)
