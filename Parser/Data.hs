{-# LANGUAGE FlexibleInstances #-}

module Parser.Data (
    Parser,
    Value(..),
    Pattern,
    Purity(..),
    Mutability,
    Visibility(..),
    Ctor(..),
    Stmt(..),
    Body,
    Expr(..),
    valPos,
) where

import Data.Array (Array, elems)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text)
import Text.Parsec.Prim (ParsecT)

import Common.SrcPos
import Common.Typing
import Common.Var
import Pretty


default (Int, Double)


type Parser a = ParsecT Text () Identity a

data Value
    = IntLit {-# UNPACK #-} !Int64 SrcPos
    | FloatLit {-# UNPACK #-} !Double SrcPos
    | CharLit {-# UNPACK #-} !Char SrcPos
    | StringLit String SrcPos
    | VarVal {-# UNPACK #-} !Var
    | Application Value [Value]
    | CtorCall {-# UNPACK #-} !Var [Value]
    | Tuple {-# UNPACK #-} !(Array Int Value)-- SrcPos
    | Array {-# UNPACK #-} !(Array Int Value)-- SrcPos
    | Lambda [Var] Value -- Body
    | StmtVal Stmt
    | Hole SrcPos
    deriving (Eq)

-- type Pattern = [Value]
type Pattern = Value

data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq)

type Mutability = Purity

data Visibility = Export | Intern
    deriving (Show, Eq)

data Ctor = Ctor {
        ctorVisib :: Visibility,
        ctorName :: {-# UNPACK #-} !Var,
        ctorTypes :: [Type]
    }
    deriving (Eq)

data Stmt
    = IfElse Value Body Body
    | Loop (Maybe Stmt) Value (Maybe Stmt) Body
    | Match Value [(Pattern, Body)]
    | NewVar Mutability Type {-# UNPACK #-} !Var Value
    | Reassignment {-# UNPACK #-} !Var Value
    | Return Value
    | ValStmt Value
    deriving (Eq)

type Body = [Stmt]

data Expr
    = FuncDecl {
        exprVisib :: Visibility,
        exprPurity :: Purity,
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
    deriving (Eq)

valPos :: Value -> SrcPos
valPos (IntLit _ p) = p
valPos (FloatLit _ p) = p
valPos (CharLit _ p) = p
valPos (StringLit _ p) = p
valPos (VarVal var) = varPos var
valPos (Application _ _) = UnknownPos -- ERROR
-- valPos (Application val vals) = case valPos val of
--     UnknownPos -> UnknownPos
--     SrcPos m ln st _ -> case valPos (tail vals) of
--         UnknownPos -> UnknownPos
--         SrcPos _ _ _ end -> SrcPos m ln st end
valPos (Lambda (par:_) val) = case varPos par of
    UnknownPos -> UnknownPos
    SrcPos m ls _ cs _ -> case valPos val of
        UnknownPos -> UnknownPos
        SrcPos _ _ le _ ce -> SrcPos m ls le cs ce
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
    terse Export = "ex"
    terse Intern = "in"
    pretty Export = "export"
    pretty Intern = "intern"
    detailed = show

instance Pretty Ctor where
    pretty (Ctor vis name types) =
        vis|+" "+|name|+"<"+|", "`seps`types|+">"

instance Pretty Value where
    terse (Tuple arr) = "("-|","`sepsT`elems arr|-")"
    terse (Array arr) = "["-|","`sepsT`elems arr|-"]"
    terse val = terse val
    pretty (IntLit n _) = show n
    pretty (FloatLit n _) = show n
    pretty (CharLit c _) = show c
    pretty (StringLit s _) = show s
    pretty (VarVal var) = pretty var
    pretty (Application val args) =
        "("+|val|+" "+|" "`seps`args|+")"
    pretty (CtorCall name args) =
        "("+|name|+" "+|" "`seps`args|+")"
    pretty (Tuple arr) = "("+|", "`seps`elems arr|+")"
    pretty (Array arr) = "[ "+|", "`seps`elems arr|+" ]"
    pretty (Lambda ps val) = " "`seps`ps|+" => "+|val
    pretty (StmtVal stmt) = "("+|stmt|+")"
    pretty (Hole _) = "_"

instance Pretty (Pattern, Body) where
    pretty (ptrn, body) = prettyPattern ptrn|+" {\n"+|indentCatLns body|+"}"

prettyPattern :: Value -> String
prettyPattern (VarVal var) = pretty var
prettyPattern (Hole _) = "_"
prettyPattern (Lambda _ _) = "(PTRN_ERR(Lambda))"
prettyPattern (StmtVal _) = "(PTRN_ERR(StmtVal))"
prettyPattern (Application _ _) = "(PTRN_ERR(Application))"
prettyPattern val = "["+|val|+"]"

instance Pretty Stmt where
    pretty (IfElse val tb fb) = "if ("+|val|+") {\n"
        +|indentCatLns tb|+"\n    else {\n"
        +|indentCatLns fb|+"}"
    pretty (Loop Nothing cond Nothing body) = 
        "loop ("+|cond|+") {\n"
        +|indentCatLns body|+"}"
    pretty (Loop mInit cond mIter body) =
        "loop ("+|mInit|+"; "+|cond|+"; "+|mIter|+") {\n"
        +|indentCatLns body|+"}"
    pretty (Match val cases) = "match ("+|val|+") {\n"
        +|indentCatLns cases|+"}"
    pretty (NewVar mut typ name val) =
        "let "+|mut|+" "+|name|+|typ|+" = "+|val|+";"
    pretty (Reassignment var val) = var|+" = "+|val|+";"
    pretty (Return val) = "return"+|val|+";"
    pretty (ValStmt val) = val|+";"

instance Pretty Expr where
    pretty (FuncDecl vis pur name typ) =
        vis|+" "+|pur|+" "+|name|+|typ
    pretty (FuncDef name pars body) =
        name|+" "+|" "`seps`params|+" {\n"+|
            indentCatLns body|+"}"
        where
            params = prettyPattern <$> pars
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
