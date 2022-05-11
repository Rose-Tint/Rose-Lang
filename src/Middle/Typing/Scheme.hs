{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Middle.Typing.Scheme (
    Scheme(..),
    TypeEnv,
    Subst,
    apply, ftv,
    extend,
    restrict,
    emptyTypeEnv,
    (<|>),
    compose,
    occurs,
    nullSubst,
) where

import Data.List (foldl')
import qualified Data.Set as S

import Common.Typing
import Common.Var
import Middle.Table.VarMap


-- http://dev.stephendiehl.com/fun/006_hindley_milner.html


-- haskell example: `forall a b. a -> b`
-- using set-symbols: ∀ab. a -> b
data Scheme = Forall [Var] Type

-- Context:
type TypeEnv = VarMap Scheme

extend :: Var -> Scheme -> TypeEnv -> TypeEnv
extend = insert

restrict :: Var -> TypeEnv -> TypeEnv
restrict = delete

emptyTypeEnv :: TypeEnv
emptyTypeEnv = empty


-- Substitution:
type Subst = VarMap Type

compose :: [Subst] -> Subst
compose = foldl' (<|>) nullSubst

infixl 8 <|>
(<|>) :: Subst -> Subst -> Subst
s1 <|> s2 = fmap (apply s1) s2 `union` s1

class Substable a where
    apply :: Subst -> a -> a
    ftv :: a -> S.Set Var

instance Substable Type where
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

instance Substable Scheme where
    apply s (Forall vars typ) =
        let s' = foldr delete s vars
        in Forall vars (apply s' typ)
    ftv (Forall vars typ) =
        ftv typ `S.difference` S.fromList vars

instance Substable a => Substable [a] where
    apply = fmap . apply
    ftv = foldr (S.union . ftv) S.empty

instance Substable TypeEnv where
    apply = (<$>) . apply
    ftv = ftv . elems


-- Unification
occurs :: Substable a => Var -> a -> Bool
occurs var a = var `S.member` ftv a

nullSubst :: Subst
nullSubst = empty
