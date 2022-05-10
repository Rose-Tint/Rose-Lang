{-# LANGUAGE FlexibleInstances #-}

module Front.Parser.Data (
    ValArray,
    Value(..),
    Purity(..),
    Mutab(..),
    Visib(..),
    Field(..),
    Ctor(..),
    Stmt(..),
    MatchCase,
    Body,
    Expr(..),
    valPos,
) where

import Data.Array (Array, elems)
import Data.Char (isAlphaNum)
import Data.Int (Int64)

import Common.SrcPos
import Common.Typing
import Common.Var
import Pretty


default (Int, Double)


type ValArray = Array Int Value

data Value
    = IntLit {-# UNPACK #-} !Int64 SrcPos
    | FloatLit {-# UNPACK #-} !Float SrcPos
    | DoubleLit {-# UNPACK #-} !Double SrcPos
    | CharLit {-# UNPACK #-} !Char SrcPos
    | StringLit String SrcPos
    | VarVal !Var
    | Application Value [Value]
    | CtorCall !Var [Value]
    | Tuple ValArray
    | Array ValArray
    | Lambda [Var] Body
    | StmtVal Stmt
    | Hole SrcPos
    deriving (Eq)

data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq)

data Mutab = Mut | Imut
    deriving (Show, Eq)

data Visib = Export | Intern
    deriving (Show, Eq)

data Field = Field !Var Type
    deriving (Eq)

data Ctor
    = Record !Var Visib [Field]
    | SumType !Var Visib [Type]
    deriving (Eq)

data Stmt
    = IfElse Value Body Body
    | Loop Stmt Stmt Stmt Body
    | Match Value [(Value, Body)]
    | NewVar Mutab !Var TypeDecl Value
    | Reassignment !Var Value
    | Return Value
    | ValStmt Value
    | Break
    | Continue
    | NullStmt
    deriving (Eq)

type MatchCase = (Value, Body)

type Body = [Stmt]

data Expr
    = FuncDecl {
        exprPurity :: Purity,
        exprVisib :: Visib,
        exprName :: !Var,
        exprType :: {-# UNPACK #-} !TypeDecl
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
    | FuncDef !Var [Value] Body
    | TypeAlias Visib !Var Type
    deriving (Eq)

valPos :: Value -> SrcPos
valPos (IntLit _ p) = p
valPos (FloatLit _ p) = p
valPos (DoubleLit _ p) = p
valPos (CharLit _ p) = p
valPos (StringLit _ p) = p
valPos (VarVal var) = varPos var
valPos (Application _ _) = UnknownPos -- ERROR
-- valPos (Application val vals) = case valPos val of
--     UnknownPos -> UnknownPos
--     SrcPos m ln st _ -> case valPos (tail vals) of
--         UnknownPos -> UnknownPos
--         SrcPos _ _ _ end -> SrcPos m ln st end
-- valPos (Lambda (par:_) val) = case varPos par of
--     UnknownPos -> UnknownPos
--     SrcPos off ln col -> case valPos val of
--         UnknownPos -> UnknownPos
--         SrcPos _ _ le _ ce -> SrcPos m ls le cs ce
valPos (Lambda _ _) = UnknownPos
valPos (CtorCall var _) = varPos var
valPos (Array _) = UnknownPos
valPos (Tuple _) = UnknownPos
valPos (StmtVal _) = UnknownPos
valPos (Hole p) = p


instance Pretty Purity where
    pretty Pure = "pure"
    pretty Impure = "impure"
    pretty Unsafe = "unsafe"
    detailed = show

instance Pretty Mutab where
    pretty Mut = "mut"
    pretty Imut = ""
    detailed = show

instance Pretty Visib where
    terse Export = "ex"
    terse Intern = "in"
    pretty Export = "export"
    pretty Intern = "intern"
    detailed = show

instance Pretty Field where
    pretty (Field name typ) = name|+|TypeDecl [] typ
    detailed (Field name typ) =
        "Field: "+|name|+|TypeDecl [] typ

instance Pretty Ctor where
    pretty (SumType vis name types) =
        vis|+" "+|name|+" "`seps`types
    pretty (Record vis name flds) =
        vis|+" "+|name|+" {\n"+|indentCatLns flds|+"\n}"
    detailed (SumType vis name types) =
        "Constructor (SumType):\n    "*|vis|*
        "\n    Name: "*|name|*
        "\n    Types:\n"+|indentCatLnsD types
    detailed (Record vis name flds) =
        "Constructor (Record):\n    "*|vis|*
        "\n    Name:"*|name|*
        "\n    Fields:\n"*|indentCatLns flds

instance Pretty Value where
    terse (FloatLit n _) = show n
    terse (VarVal var) = terse var
    terse (Tuple arr) = "("-|","`sepsT`elems arr|-")"
    terse (Array arr) = "["-|","`sepsT`elems arr|-"]"
    terse val = pretty val
    pretty (IntLit n _) = show n
    pretty (FloatLit n _) = show n ++ "f"
    pretty (DoubleLit n _) = show n
    pretty (CharLit c _) = show c
    pretty (StringLit s _) = show s
    pretty (VarVal (Var name _))
        | all isIdChar name = name
        | otherwise = "("+|name|+")"
        where
            isIdChar c = isAlphaNum c || c == '_'
    pretty (Application val args) =
        "("+|val|+" "+|" "`seps`args|+")"
    pretty (CtorCall name args) =
        "("+|name|+" "+|" "`seps`args|+")"
    pretty (Tuple arr) = "("+|", "`seps`elems arr|+")"
    pretty (Array arr) = "[ "+|", "`seps`elems arr|+" ]"
    pretty (Lambda ps stmts) =  " "`seps`ps|+" => {\n"
        +|indentCatLns stmts|+"}"
    pretty (StmtVal stmt) = "("+|stmt|+")"
    pretty (Hole _) = "_"

instance Pretty (Value, Body) where
    pretty (ptrn, body) = prettyPtrn ptrn|+
        " {\n"+|indentCatLns body|+"}"

prettyPtrn :: Value -> String
prettyPtrn (VarVal var) = pretty var
prettyPtrn (Hole _) = "_"
prettyPtrn (Lambda _ _) = "(PTRN_ERR(Lambda))"
prettyPtrn (StmtVal _) = "(PTRN_ERR(StmtVal))"
prettyPtrn (Application _ _) = "(PTRN_ERR(Application))"
prettyPtrn val = "["+|val|+"]"

instance Pretty Stmt where
    pretty (IfElse val tb fb) = "if ("+|val|+") {\n"
        +|indentCatLns tb|+"\n    else {\n"
        +|indentCatLns fb|+"}"
    pretty (Loop NullStmt cond NullStmt body) =
        "loop ("+|cond|+") {\n"
        +|indentCatLns body|+"}"
    pretty (Loop init' cond iter body) =
        "loop ("+|init'|+"; "+|cond|+"; "+|iter|+") {\n"
        +|indentCatLns body|+"}"
    pretty (Match val cases) = "match ("+|val|+") {\n"
        +|indentCatLns cases|+"}"
    pretty (NewVar mut name typ val) =
        "let "+|mut|+" "+|name|+|typ|+" = "+|val|+";"
    pretty (Reassignment var val) = var|+" = "+|val|+";"
    pretty Continue = "continue"
    pretty Break = "break"
    pretty (Return val) = "return "+|val|+";"
    pretty (ValStmt val) = val|+";"
    pretty NullStmt = ";"
    detailed (IfElse val tb []) =
        "If: "*|val
            |*|indentLns (indentCatLnsD tb)|+
        "Else:"
    detailed (IfElse val tb fb) =
        "If: "*|val
            |*|indentLns (indentCatLnsD tb)|+
        "Else:\n"
            +|indentLns (indentCatLnsD fb)
    detailed (Loop init' cond iter body) =
        "Loop:\n    Initial: "*|init'|*
        "\n    Condition: "*|cond|*
        "\n    Iteration: "*|iter|*
        "\n    Body:\n"
            +|indentLns (indentCatLnsD body)
    detailed (Match val cases) =
        "Match: "*|val|*"\n"+|
            (indentCatLns $ fmap ("Case: "*|) cases)
    detailed (NewVar mut name typ val) =
        "New Variable:\n    Mutab.: "*|mut|*
        "\n    Type: "*|typ|*
        "\n    Name: "+|name|+
        "\n    Value: "*|val
    detailed (Reassignment var val) =
        "Reassignment:\n    Variable: "*|var|*
        "\n    Value: "*|val
    detailed Continue = "Continue"
    detailed Break = "Break"
    detailed (Return val) = "Return: "*|val
    detailed (ValStmt val) =
        "Expr: "*|val
    detailed NullStmt = "Null Statement"

instance Pretty Expr where
    pretty (FuncDecl vis pur name typ) =
        vis|+" "+|pur|+" "+|name|+|typ
    pretty (FuncDef name pars body) =
        name|+" "+|" "`seps`fmap prettyPtrn pars|+" {\n"+|
            indentCatLns body|+"}"
    pretty (DataDef vis name pars []) =
        vis|+" data "+|name|+" "+|" "`seps`pars
    pretty (DataDef vis name pars (ctor:ctors)) =
        vis|+" data "+|name|+" "+|" "`seps`pars|+
        indentLns ("= "+|ctor)|+|
        indentCatLns (fmap ("| "+|) ctors)
    pretty (TraitDecl vis ctx name pars fns) =
        vis|+" trait <"+|", "`seps`ctx|+"> "+|name|+
        " "+|" "`seps`pars|+
        " {\n"+|indentCatLns fns|+"}"
    pretty (TraitImpl ctx name types fns) =
        "impl <"+|", "`seps`ctx|+"> "+|name|+
        " "+|" "`seps` types|+
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
