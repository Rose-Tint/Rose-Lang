{-# LANGUAGE FunctionalDependencies #-}

module CodeGen.LLVM.ToLLVM (ToLLVM(..)) where

import qualified LLVM.AST as LLVM

import Utils.String (strToShortBS)


class ToLLVM a b | b -> a where
    toLLVM :: a -> b


instance ToLLVM Visibility LLVM.Linkage where
    toLLVM Intern = LLVM.Private
    toLLVM Export = LLVM.External

instance ToLLVM Type LLVM.Type where
    toLLVM (Type _ typeArgs) = LLVM.StructureType {
            LLVM.isPacked = False,
            LLVM.elementTypes = toLLVM <$> typeArgs
        }
    toLLVM (Param _ typeArgs) = LLVM.StructureType {
            LLVM.isPacked = False,
            LLVM.elementTypes = toLLVM <$> typeArgs
        }
    toLLVM (Applied []) = error
        "ToLLVM.toLLVM: empty `Applied` type"
    toLLVM (Applied [t]) = toLLVM t
    toLLVM (Applied types) = LLVM.FunctionType {
        resultType = rT,
        argumentTypes = pTs,
        isVarArg = False
        }
        where
            (pTs, rT) = splitAt (length types - 2) types
    toLLVM Delayed = error
        "ToLLVM.toLLVM: invalid `Delayed` type"
    toLLVM NoType = error
        "ToLLVM.toLLVM: invalid `NoType` type"

instance ToLLVM Stmt LLVM.Instruction where
    toLLVM (IfElse cond tb fb) = CondBr {
        condition = toLLVM cond,
        trueDest = {- Name -},
        falseDest = {- Name -},
        metadata' = []
        }
    toLLVM (Return val) = Ret (toLLVM val) []
    toLLVM _ = error
        "ToLLVM.toLLVM: Stmt not fully implemented"

{-
    = IfElse Value Body Body
    | Loop Stmt Stmt Stmt Body
    | Match Value [(Value, Body)]
    | NewVar Mutability !Var TypeDecl Value
    | Reassignment !Var Value
    | ValStmt Value
    | Break
    | Continue
    | NullStmt
-}


instance ToLLVM Var LLVM.Name where
    toLLVM = Name . strToShortBS . varName

instance ToLLVM Expr LLVM.Global where
    toLLVM (FuncDecl pur vis name typ) =
        LLVM.functionDefaults {
            LLVM.linkage = toLLVM vis,
            LLVM.callingConvention = CC.GHC,
            LLVM.returnType = toLLVM <$> typ,
            LLVM.name = toLLVM name,

             -- what is this?
            -- LLVM.parameters = ([], False),

            -- TODO? Maybe for definitions?
            -- LLVM.basicBlocks = []
        }
    toLLVM (TypeAlias vis alias typ) =
        error "ToLLVM.toLLVM: `TypeAlias`"
        -- LLVM.globalAliasDefaults {
        --     LLVM.name = toLLVM name,
        --     LLVM.linkage = toLLVM vis,
        --     LLVM.type' =
        --     LLVM.aliasee =
        -- }
    toLLVM _ = error
        "ToLLVM.toLLVM: `Expr` not fully implemented"