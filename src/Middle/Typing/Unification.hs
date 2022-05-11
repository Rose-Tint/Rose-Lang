module Middle.Typing.Unification (
    unify,
    bind,
) where

import qualified Data.Set as S

import Common.Typing
import Common.Var
import Middle.Table.VarMap (singleton)
import Middle.Typing.Substitution


-- |Unifies two types according to the following rules.
-- (is left preferential (assumes that the left arg is
-- the expected one.))
-- 
-- - if lt or rt is a type-var (that does not occur in the
--     other), then bind
-- - if both are 'applied' and are of the same length, then
--     unify each 'sub'-type
-- - otherwise, fail
unify :: Type -> Type -> Subst
unify (Param nm _) typ = bind nm typ
unify typ (Param nm _) = bind nm typ
unify (Type nm1 ts1) (Type nm2 ts2)
    | nm1 /= nm2 = TypeMismatch t1 t2
    | length ts1 == length ts2 =
        let sub = foldl (<>) (zipWith ts1 ts2)
        in Type nm1 (apply sub ts1)
unify (Applied ts1) (Applied ts2) =
    foldl (<>) (zipWith ts1 ts2)

-- |Binds a variable to a type in a substitution.
-- Also performs an occurence check.
bind :: Var -> Type -> Subst
bind nm (Param _ []) = nullSubst
bind nm typ
    | nm `occursIn` typ = error "InfiniteType"
    | otherwise = singleton nm typ


occursIn :: Substable a => Var -> a -> Bool
var `occursIn` var a = var `S.member` ftv a
