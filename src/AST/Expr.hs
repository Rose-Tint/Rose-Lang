module AST.Expr (
    Expr(..),
) where

import AST.Ctor
import AST.Stmt
import AST.Pattern
import Common.Var
import Common.Specifiers
import Text.Pretty
import Typing.Constraint
import Typing.Type
import Typing.TypeDecl


data Expr
    = FuncDecl {
        exprPurity :: Purity,
        exprVisib :: Visib,
        exprName :: !Var,
        exprType :: !TypeDecl
    }
    | DataDef {
        exprVisib :: Visib,
        exprName :: !Var,
        exprParams :: [Var],
        exprCtors :: [Ctor]
    }
    | TraitDecl {
        exprVisib :: Visib,
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
    | FuncDef !Var [Pattern] Body
    | TypeAlias Visib !Var Type


instance Pretty Expr where
    pretty (FuncDecl vis pur name typ) =
        vis|+" "+|pur|+" "+|name|+|typ
    pretty (FuncDef name pars body) =
        name|+" "+|" "`seps`pars|+
            " {\n"+|indentCatLns body|+"}"
    pretty (DataDef vis name pars []) =
        vis|+" data "+|name|+" "+|" "`seps`pars
    pretty (DataDef vis name pars (ctor:ctors)) =
        vis|+" data "+|name|+" "+|" "`seps`pars|+
        indentLns ("= "+|ctor)|+|
        indentCatLns (fmap ("| "+|) ctors)
    pretty (TraitDecl vis ctx name pars fns) =
        vis|+" trait<"+|init(init(pretty ctx))|+"> "
        +|name|+" "+|" "`seps`pars|+
        " {\n"+|indentCatLns fns|+"}"
    pretty (TraitImpl ctx name types fns) =
        "impl <"+|init(init(pretty ctx))|+"> "
        +|name|+" "+|" "`seps` types|+
        " {\n"+|indentCatLns fns|+"}"
    pretty (TypeAlias vis alias typ) =
        vis|+" using "+|alias|+" = "+|typ
    detailed (FuncDecl vis pur name typ) =
        "Function Declaration:\n    Visib.: "*|vis|*
        "\n    Purity: "*|pur|*
        "\n    Name: "*|name|*
        "\n    Type: "*|typ
    detailed (FuncDef name pars body) =
        "Function Definition:\n    Name: "*|name|*
        "\n    Params: "*|","`sepsD`pars|*
        "\n    Body:\n"*|indentLns (indentCatLnsD body)
    detailed (DataDef vis name pars ctors) =
        "Datatype Definition:\n    Visib.: "*|vis|*
        "\n    Name: "*|name|*
        "\n    Type-Vars: "+|", "`sepsD`pars|+
        "\n    Constructors:\n"
            +|indentLns (indentCatLnsD ctors)
    detailed (TraitDecl vis ctx name pars fns) =
        "Trait Declaration:\n    Visib.: "*|vis|*
        "\n    Context: "*|ctx|*
        "\n    Name: "*|name|*
        "\n    Type-Vars: "+|", "`sepsD`pars|+
        "\n    Methods:\n"
            +|indentLns (indentCatLnsD fns)
    detailed (TraitImpl ctx name types fns) =
        "Trait Impl.:\n    Context: "*|ctx|*
        "\n    Name: "*|name|*
        "\n    Types:\n"
            +|indentLns (indentCatLnsD types)|+
          "    Methods:\n"
            +|indentLns (indentCatLnsD fns)
    detailed (TypeAlias vis name typ) =
        "Type Alias:\n    Visib.: "*|vis|*
        "\n    Name: "*|name|*
        "\n    Type: "*|typ
