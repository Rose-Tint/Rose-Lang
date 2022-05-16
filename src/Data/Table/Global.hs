{-# LANGUAGE FlexibleInstances #-}

module Data.Table.Global (
    Global(..),
    glbPurity,
    funcToMeth,
) where

import Common.SrcPos
import Common.Specifiers
import Common.Var
import Text.Pretty


data Global
    = Function {
        -- glbType :: TypeDecl,
        glbVisib :: Visib,
        globalPurity :: Maybe Purity,
        _glbPos :: SrcPos
    }
    | Constructor {
        -- glbType :: TypeDecl,
        glbVisib :: Visib,
        glbParent :: Var,
        _glbPos :: SrcPos
    }
    | Method {
        -- glbType :: TypeDecl,
        glbVisib :: Visib,
        globalPurity :: Maybe Purity,
        glbParent :: Var,
        _glbPos :: SrcPos
    }
    -- deriving (Eq)


glbPurity :: Global -> Maybe Purity
glbPurity (Function _ pur _) = pur
glbPurity (Constructor _ _ _) = Nothing
glbPurity (Method _ pur _ _) = pur

funcToMeth :: Global -> Var -> Global
funcToMeth (Function vis pur _) trt =
    Method vis pur trt (getPos trt)
funcToMeth _ _ = error "funcToMeth: non-`Function` argument."


instance Pretty Global where
    pretty = detailed
    detailed (Function vis pur pos) =
         "| "+|9.>terse pos|+
        " | "+|6.>vis|+
        " | "+|6.>pur|+
        -- " | "+|35.>typ|+
        " |"
    detailed (Constructor vis par pos) =
         "| "+|9.>terse pos|+
        " | "+|6.>vis|+
        " | "+|6.>par|+
        -- " | "+|35.>typ|+
        " |"
    detailed (Method vis pur par pos) =
         "| "+|9.>terse pos|+
        " | "+|6.>vis|+
        " | "+|6.>pur|+
        -- " | "+|35.>typ|+
        " | (parent = "+|par|+")"

instance HasSrcPos Global where
    getPos = _glbPos
