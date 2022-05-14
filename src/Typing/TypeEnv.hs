module Typing.TypeEnv (
    TypeEnv,
    extend,
    restrict,
    emptyEnv,
) where

import Prelude hiding (lookup)

import Common.Var
import Data.VarMap
import Typing.Scheme


type TypeEnv = VarMap Scheme


extend :: Var -> Scheme -> TypeEnv -> TypeEnv
extend = insert

restrict :: Var -> TypeEnv -> TypeEnv
restrict = delete

-- TODO: 'empty' contains primatives
emptyEnv :: TypeEnv
emptyEnv = empty
