module AST.Expr (
    Expr(..),
    Ctor(..),
) where

import AST.Stmt
import AST.Pattern
import Common.Var
import Common.Specifiers
import Common.SrcPos
import Text.Pretty
import Typing.Constraint
import Typing.Type


data Expr
    = FuncDecl {
        exprPurity :: Purity,
        exprName :: {-# UNPACK #-} !Var,
        exprType :: !Type
    }
    | FuncDef {
        exprName :: !Var,
        exprParams :: [Pattern],
        exprBody :: Stmt
    }
    | DataDef {
        exprName :: {-# UNPACK #-} !Var,
        exprTypeVars :: [Var],
        exprCtors :: [Ctor]
    }
    | TraitDecl {
        exprCtx :: Context,
        exprName :: {-# UNPACK #-} !Var,
        exprTypeVars :: [Var],
        exprFuncs :: [Expr]
    }
    | TraitImpl {
        exprCtx :: Context,
        exprName :: {-# UNPACK #-} !Var,
        exprTypes :: [Type],
        exprFuncs :: [Expr]
    }

data Ctor
    = SumType {
        ctorName :: {-# UNPACK #-} !Var,
        sumTypes :: [Type]
    }


instance HasSrcPos Ctor where
    getPos = getPos . ctorName

instance HasSrcPos Expr where
    getPos = getPos . exprName

instance Pretty Ctor where
    pretty (SumType name types) =
        name|+" "`seps`types
    detailed (SumType name types) =
        "Constructor:\n    Name: "*|name|*
        "\n    Types:\n"+|indentCatLnsD types

instance Pretty Expr where
    pretty (FuncDecl pur name typ) =
        pur|+" "+|name|+|typ
    pretty (FuncDef name pars body) =
        name|+" "+|" "`seps`pars|+" "+|body
    pretty (DataDef name pars []) =
        " data "+|name|+" "+|" "`seps`pars
    pretty (DataDef name pars (ctor:ctors)) =
        " data "+|name|+" "+|" "`seps`pars|+
        indentLns ("= "+|ctor)|+|
        indentCatLns (fmap ("| "+|) ctors)
    pretty (TraitDecl ctx name pars fns) =
        " trait<"+|init(init(pretty ctx))|+"> "
        +|name|+" "+|" "`seps`pars|+
        " {\n"+|indentCatLns fns|+"}"
    pretty (TraitImpl ctx name types fns) =
        "impl <"+|init(init(pretty ctx))|+"> "
        +|name|+" "+|" "`seps` types|+
        " {\n"+|indentCatLns fns|+"}"
    detailed (FuncDecl pur name typ) =
        "Function Declaration:\n    Purity: "*|pur|*
        "\n    Name: "*|name|*
        "\n    Type: "*|typ
    detailed (FuncDef name pars body) =
        "Function Definition:\n    Name: "*|name|*
        "\n    Params: "*|","`sepsD`pars|*
        "\n    Body:\n"*|indentLns (indentLnsD body)
    detailed (DataDef name pars ctors) =
        "Datatype Definition:\n    Name: "*|name|*
        "\n    Type-Vars: "+|", "`sepsD`pars|+
        "\n    Constructors:\n"
            +|indentLns (indentCatLnsD ctors)
    detailed (TraitDecl ctx name pars fns) =
        "Trait Declaration:\n    Context:"*|ctx|*
        "\n    Name: "*|name|*
        "\n    Type-Vars: "+|", "`sepsD`pars|+
        "\n    Methods:\n"
            +|indentLns (indentCatLnsD fns)
    detailed (TraitImpl ctx name types fns) =
        "Trait Impl.:\n    Context:"*|ctx|*
        "\n    Name: "*|name|*
        "\n    Types:\n"
            +|indentLns (indentCatLnsD types)|+
          "    Methods:\n"
            +|indentLns (indentCatLnsD fns)
