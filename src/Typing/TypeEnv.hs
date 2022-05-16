module Typing.TypeEnv (
    TypeEnv,
    extend,
    restrict,
    emptyEnv,
) where

import Prelude hiding (lookup)

import Common.Var
import Data.VarMap
import Typing.Primitives
import Typing.Scheme
import Typing.Type


type TypeEnv = VarMap Scheme


extend :: Var -> Scheme -> TypeEnv -> TypeEnv
extend = insert

restrict :: Var -> TypeEnv -> TypeEnv
restrict = delete

-- TODO: 'empty' contains primatives
emptyEnv :: TypeEnv
emptyEnv = fromList [
    (prim "Bool", Forall [] boolType),
    (prim "Int", Forall [] intType),
    (prim "Float", Forall [] floatType),
    (prim "Double", Forall [] doubleType),
    (prim "String", Forall [] stringType),
    (prim "Char", Forall [] charType),
    (prim "[]", Forall [prim "a"] (arrayOf (TypeVar (prim "a")))),
    (prim "(,)", Forall [prim "a"] (arrayOf (TypeVar (prim "a"))))
    ]
