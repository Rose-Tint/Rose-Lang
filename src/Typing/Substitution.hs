module Typing.Substitution (
    Subst,
    compose,
    (<|>),
    apply,
    ftv,
) where

import qualified Data.Set as S

import Data.VarMap


type Subst = VarMap Type

nullSubst :: Subst
nullSubst = empty

compose :: [Subst] -> Subst
compose = foldl' (<|>) nullSubst

infixl 8 <|>
(<|>) :: Subst -> Subst -> Subst
s1 <|> s2 = fmap (apply s1) s2 `union` s1

class Substitutable a where
    apply :: Subst -> a -> a
    -- | (f)ree (t)ype (v)ariables
    ftv :: a -> S.Set Var

instance Substitutable Type where
    apply s (Type nm types) =
        Type nm (apply s <$> types)
    apply s (Param nm types) = findWithDefault
        (Param nm (apply s types)) nm s
    apply s (Applied types) = Applied (apply s <$> types)
    ftv (Type _ types) = foldr
        S.union S.empty (ftv <$> types)
    ftv (Param nm types) = foldr
        S.union (S.singleton nm) (ftv <$> types)
    ftv (Applied types) = foldr
        S.union S.empty (ftv <$> types)

instance Substitutable Scheme where
    apply s (Forall vars typ) =
        -- if a type-var appears (is quantified) in
        -- the scheme, do not substitute it.
        let s' = foldr delete s vars
        in Forall vars (apply s' typ)
    ftv (Forall vars typ) =
        ftv typ `S.difference` S.fromList vars

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (S.union . ftv) S.empty

instance Substitutable TypeEnv where
    apply = (<$>) . apply
    ftv = ftv . elems
