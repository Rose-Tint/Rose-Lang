{-# LANGUAGE FlexibleInstances #-}

module Middle.Table.Data.Global (
    Global(..),
    glbPurity,
    funcToMeth,
) where

import Common.Var
import Common.SrcPos
import Common.Typing
import Front.Parser
import Pretty


data Global
    = Function {
        glbType :: TypeDecl,
        glbVisib :: Visib,
        globalPurity :: Maybe Purity,
        glbPos :: SrcPos
    }
    | Constructor {
        glbType :: TypeDecl,
        glbVisib :: Visib,
        glbParent :: Var,
        glbPos :: SrcPos
    }
    | Method {
        glbType :: TypeDecl,
        glbVisib :: Visib,
        globalPurity :: Maybe Purity,
        glbParent :: Var,
        glbPos :: SrcPos
    }
    deriving (Eq)


glbPurity :: Global -> Maybe Purity
glbPurity (Function _ _ pur _) = pur
glbPurity (Constructor _ _ _ _) = Nothing
glbPurity (Method _ _ pur _ _) = pur

funcToMeth :: Global -> Var -> Global
funcToMeth (Function typ vis pur _) trt =
    Method typ vis pur trt (varPos trt)
funcToMeth _ _ = error "funcToMeth: non-`Function` argument."


instance Pretty Global where
    pretty = detailed
    detailed (Function typ vis pur pos) =
         "| "+|9.>terse pos|+
        " | "+|6.>vis|+
        " | "+|6.>pur|+
        " | "+|35.>typ|+
        " |"
    detailed (Constructor typ vis par pos) =
         "| "+|9.>terse pos|+
        " | "+|6.>vis|+
        " | "+|6.>par|+
        " | "+|35.>typ|+
        " |"
    detailed (Method typ vis pur par pos) =
         "| "+|9.>terse pos|+
        " | "+|6.>vis|+
        " | "+|6.>pur|+
        " | "+|35.>typ|+
        " | (parent = "+|par|+")"
