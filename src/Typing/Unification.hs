module Typing.Unification (
    unify,
    bind,
) where

import Control.Monad (zipWithM)

import Analysis.Error
import Common.Var
import Data.VarMap (singleton)
import Typing.Infer
import Typing.Type
import Typing.Substitution


-- |Unifies two types according to the following rules.
-- (is left preferential (assumes that the left arg is
-- the expected one.))
--
-- - if lt or rt is a type-var (that does not occur in the
--     other), then bind
-- - if both are 'applied' and are of the same length, then
--     unify each 'sub'-type
-- - otherwise, fail
unify :: Type -> Type -> Infer Subst
unify (TypeVar nm) typ = bind nm typ
unify typ (TypeVar nm) = bind nm typ
unify t1@(Type nm1 ts1) t2@(Type nm2 ts2)
    | nm1 /= nm2 = throw (TypeMismatch t1 t2)
    | length ts1 == length ts2 =
        compose <$> (zipWithM unify ts1 ts2)
unify (l1 :-> l2) (r1 :-> r2) = do
    s1 <- unify l1 r1
    s2 <- unify (apply s1 l2) (apply s1 r2)
    return (s2 <|> s1)
unify (ArrayType t1) (ArrayType t2) = unify t1 t2
unify (TupleType ts1) (TupleType ts2) =
    compose <$> (zipWithM unify ts1 ts2)
unify t1 t2 = throw (TypeMismatch t1 t2)

-- |Binds a variable to a type in a substitution.
-- Also performs an occurence check.
bind :: Var -> Type -> Infer Subst
bind _ (TypeVar _) = return nullSubst
bind nm typ
    | nm ~> typ = throw (InfiniteType nm typ)
    | otherwise = return (singleton nm typ)
