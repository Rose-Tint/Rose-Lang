module Typing.TypeEnv {-(
    TypeEnv(..),
    fromTable,
    extendTypes,
    extendGlobals,
    extendScopeds,
    addScope,
    remScope,
    emptyEnv,
)-} where{-

import Prelude hiding (lookup)

import Common.Var
import Data.Table hiding (addScope, remScope)
import Data.VarMap as M
import Typing.Primitives
import Typing.Scheme
import Typing.Type


data TypeEnv = Env {
    envTypes :: VarMap Scheme,
    envGlobals :: VarMap Scheme,
    envScopeds :: [VarMap Scheme]
    }


fromTable :: Table -> TypeEnv
fromTable tbl = Env {
    envTypes = dataScheme <$> tblTypes tbl,
    envGlobals = funcScheme <$> tblGlobals tbl,
    envScopeds = fmap funcScheme <$> tblScopeds tbl
    }

extendTypes :: Var -> Scheme -> TypeEnv -> TypeEnv
extendTypes var scheme env = env {
    envTypes = M.insert var scheme (envTypes env)
    }

extendGlobals :: Var -> Scheme -> TypeEnv -> TypeEnv
extendGlobals var scheme env = env {
    envGlobals = M.insert var scheme (envGlobals env)
    }

extendScopeds :: Var -> Scheme -> TypeEnv -> TypeEnv
extendScopeds name scheme env = case envScopeds env of
    [] -> extendGlobals name scheme env
    (scp:scps) -> env {
        envScopeds = (M.insert name scheme scp:scps)
        }

addScope :: TypeEnv -> TypeEnv
addScope env = env { envScopeds = (M.empty:envScopeds env) }

remScope :: TypeEnv -> TypeEnv
remScope env = case envScopeds env of
    [] -> env
    (_:scps) -> env { envScopeds = scps }

-- TODO: 'empty' contains primatives
emptyEnv :: TypeEnv
emptyEnv = Env (fromList [
    (prim "Bool", Forall [] boolType),
    (prim "Int", Forall [] intType),
    (prim "Float", Forall [] floatType),
    (prim "Double", Forall [] doubleType),
    (prim "String", Forall [] stringType),
    (prim "Char", Forall [] charType),
    (prim "[]", Forall [prim "a"] (arrayOf (TypeVar (prim "a")))),
    (prim "(,)", Forall [prim "a"] (arrayOf (TypeVar (prim "a"))))
    ]) M.empty []
-}