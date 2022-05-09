{-# LANGUAGE LambdaCase #-}

module Middle.Analyzer.Validator (

) where

import Analyzer.Checker
import Analyzer.Error
import Analyzer.Internal
import Analyzer.SymbolTable


class Validator a where
    validate :: a -> Analyzer a
    validate = id


instance Validator Expr where
    valid (FuncDecl pur vis name (TypeDecl _ typ)) =
        define name $! lookupGlobal name >>= \case
            Nothing ->
                pushGlobal name $! mkSymbolData
                    

{-
    = FuncDecl {
        exprPurity :: Purity,
        exprVisib :: Visibility,
        exprName :: !Var,
        exprType :: {-# UNPACK #-} !TypeDecl
    }
    | DataDef {
        exprVisib :: Visibility,
        exprName :: !Var,
        exprParams :: [Var],
        exprCtors :: [Ctor]
    }
    | TraitDecl {
        exprVisib :: Visibility,
        exprCtx :: Context,
        exprName :: !Var,
        exprParams :: [Var],
        exprFuncs :: [Expr]
    }
    | TraitImpl {
        exprCtx :: Context,
        exprName :: !Var,
        exprTypes :: [Type],
        exprFuncs :: [Expr]
    }
    | FuncDef !Var [Value] Body
    | TypeAlias Visibility !Var Type
-}
