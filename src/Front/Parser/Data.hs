{-# LANGUAGE FlexibleInstances #-}

module Front.Parser.Data (
    Value(..),
    Purity(..),
    Mutability,
    Visibility(..),
    Field(..),
    Ctor(..),
    Stmt(..),
    MatchCase,
    Body,
    Expr(..),
    valPos,
) where

import Data.Array (Array, elems)
import Data.Int (Int64)

import Common.SrcPos
import Common.Typing
import Common.Var
import Pretty


default (Int, Double)


data Value
    = IntLit {-# UNPACK #-} !Int64 SrcPos
    | FloatLit {-# UNPACK #-} !Float SrcPos
    | DoubleLit {-# UNPACK #-} !Double SrcPos
    | CharLit {-# UNPACK #-} !Char SrcPos
    | StringLit String SrcPos
    | VarVal {-# UNPACK #-} !Var
    | Application Value [Value]
    | CtorCall {-# UNPACK #-} !Var [Value]
    | Tuple {-# UNPACK #-} !(Array Int Value)
    | Array {-# UNPACK #-} !(Array Int Value)
    | Lambda [Var] Body
    | StmtVal Stmt
    | Hole SrcPos
    deriving (Eq)

data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq)

type Mutability = Purity

data Visibility = Extern | Intern
    deriving (Show, Eq)

data Field = Field {-# UNPACK #-} !Var Type
    deriving (Eq)

data Ctor
    = Record
        {-# UNPACK #-} !Var
        Visibility
        [Field]
    | SumType
        {-# UNPACK #-} !Var
        Visibility
        [Type]
    deriving (Eq)

data Stmt
    = IfElse Value Body Body
    | Loop Stmt Stmt Stmt Body
    | Match Value [(Value, Body)]
    | NewVar
        Mutability
        {-# UNPACK #-} !Var
        Type
        Value
    | Reassignment {-# UNPACK #-} !Var Value
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
        exprVisib :: Visibility,
        exprName :: {-# UNPACK #-} !Var,
        exprType :: {-# UNPACK #-} !TypeDecl
    }
    | FuncDef {
        exprName :: {-# UNPACK #-} !Var,
        exprPars :: [Value],
        exprBody :: Body
    }
    | DataDef {
        exprVisib :: Visibility,
        exprName :: {-# UNPACK #-} !Var,
        exprParams :: [Var],
        exprCtors :: [Ctor]
    }
    | TraitDecl {
        exprVisib :: Visibility,
        exprCtx :: Context,
        exprName :: {-# UNPACK #-} !Var,
        exprParams :: [Var],
        exprFuncs :: [Expr]
    }
    | TraitImpl {
        exprCtx :: Context,
        exprName :: {-# UNPACK #-} !Var,
        exprTypes :: [Type],
        exprFuncs :: [Expr]
    }
    | TypeAlias Visibility Type Type
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
    terse Pure = "pu"
    terse Impure = "im"
    terse Unsafe = "un"
    pretty Pure = "pure"
    pretty Impure = "impure"
    pretty Unsafe = "unsafe"

instance Pretty Visibility where
    terse Extern = "ex"
    terse Intern = "in"
    pretty Extern = "export"
    pretty Intern = "intern"
    detailed = show

instance Pretty Field where
    pretty (Field name typ) = name|+|TypeDecl [] typ

instance Pretty Ctor where
    pretty (SumType vis name types) =
        vis|+" "+|name|+" "`seps`types
    pretty (Record vis name flds) =
        vis|+" "+|name|+" {\n"+|indentCatLns flds|+"\n}"

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
    pretty (VarVal var) = pretty var
    pretty (Application val args) =
        "("+|val|+" "+|" "`seps`args|+")"
    pretty (CtorCall name args) =
        "("+|name|+" "+|" "`seps`args|+")"
    pretty (Tuple arr) = "("+|", "`seps`elems arr|+")"
    pretty (Array arr) = "[ "+|", "`seps`elems arr|+" ]"
    pretty (Lambda ps stmts) = " "`seps`ps|+" => "+|indentCatLns stmts
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
    pretty (NewVar mut typ name val) =
        "let "+|mut|+" "+|name|+|typ|+" = "+|val|+";"
    pretty (Reassignment var val) = var|+" = "+|val|+";"
    pretty Continue = "continue"
    pretty Break = "Break"
    pretty (Return val) = "return"+|val|+";"
    pretty (ValStmt val) = val|+";"
    pretty NullStmt = ";"

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
