module Parser.Data (
    Var(..),
    Position(..),
    Constraint(..),
    Type(..),
    TypeDecl(..),
    Value(..),
    Pattern,
    Purity(..),
    Mutability,
    Visibility(..),
    Ctor(..),
    Stmt(..),
    Body,
    Expr(..),
    prim,
    boolType,
    valPos,
    srcModule, srcLine, srcStart, srcEnd,
    posModule, posLine, posStart, posEnd,
    newPosition,
) where

import Data.Array (Array)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty, toList)
import Text.Parsec.Pos

import Color
import Pretty
import Utils


default (Int, Double)


data Var = Var {
        varName :: String,
        varPos :: Position
    }
    deriving (Show)

data Position
    = UnknownPos
    | SourcePos {
        srcModule :: {-# UNPACK #-} !Var,
        srcLine :: {-# UNPACK #-} !Line,
        srcStart :: {-# UNPACK #-} !Column,
        srcEnd{-# UNPACK #-} !Column
    }
    deriving (Show, Eq, Ord)

data Constraint = Constraint {-# UNPACK #-} !Var [Var]
    deriving (Show, Eq, Ord)

data Type
    = Type {-# UNPACK #-} !Var [Type]
    | Applied [Type]
    -- | Param {-# UNPACK #-} !Var [Type]
    | Delayed
    | NoType
    deriving (Show, Eq)

data TypeDecl = TypeDecl [Constraint] Type
    deriving (Show, Eq)

data Value
    -- strictness is because it would not be evaluated
    -- for a long time
    = IntLit {-# UNPACK #-} !Int Position
    | FltLit {-# UNPACK #-} !Double Position
    | ChrLit {-# UNPACK #-} !Char Position
    | StrLit String Position
    | FuncCall {-# UNPACK #-} !Var [Value]
    | CtorVal {-# UNPACK #-} !Var [Value]
    | Tuple {-# UNPACK #-} !(Array Int Value) Position
    | Array {-# UNPACK #-} !(Array Int Value) Position
    | Lambda [Var] Expr -- Body
    | StmtVal Expr
    | Hole Position
    deriving (Show, Eq, Ord)

type Pattern = [Value]

data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq, Ord)

type Mutability = Purity

data Visibility = Export | Intern
    deriving (Show, Eq, Ord)

data Ctor = Ctor {
        ctorVisib :: Visibility,
        ctorName :: {-# UNPACK #-} !Var,
        ctorTypes :: [Type]
    }
    deriving (Show, Eq, Ord)

data Stmt
    = IfElse {
        stmtClause :: Value,
        stmtTrue :: Body,
        stmtFalse :: Body
    }
    | Loop {
        stmtInit :: Maybe Expr,
        stmtCond :: Value,
        stmtIter :: Maybe Expr,
        stmtBody :: Body
    }
    | Match Value [(Pattern, Body)]
    | NewVar Mutability Type {-# UNPACK #-} !Var Value
    | Reassign {-# UNPACK #-} !Var Value
    | Return Value
    | ValueS Value

type Body = [Stmt]

data Expr
    = Pragma Pragma
    | FuncDecl {
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
        exprTypePars :: [Var],
        exprCtors :: [Ctor]
    }
    | TraitDecl {
        exprVisib :: Visibility,
        exprCons :: [Constraint],
        exprName :: {-# UNPACK #-} !Var,
        exprTypeVar :: {-# UNPACK #-} !Var,
        exprFuncs :: [Expr]
    }
    | TraitImpl {
        exprName :: {-# UNPACK #-} !Var,
        exprType :: {-# UNPACK #-} !TypeDecl,
        exprFuncs :: [Expr]
    }
    deriving (Show, Eq, Ord)


mkPos :: SourcePos -> Position
mkPos pos = SourcePos
            (prim $! sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            (sourceColumn pos)

prim :: String -> Var
{-# INLINE prim #-}
prim s = Var s UnknownPos

boolType :: Type
{-# INLINE boolType #-}
boolType = TerminalType (prim "Boolean") []

valPos :: Value -> Position
{-# INLINE valPos #-}
valPos (IntLit _ p) = p
valPos (FltLit _ p) = p
valPos (ChrLit _ p) = p
valPos (StrLit _ p) = p
valPos (FuncCall var _) = varPos var
valPos (CtorVal var _) = varPos var
valPos (Array _ p) = p
valPos (StmtVal _) = UnknownPos
valPos (Hole p) = p

posModule :: Position -> Module
{-# INLINE posModule #-}
posModule UnknownPos = UnknownMod
posModule (SourcePos nm _ _ _) = nm

posLine :: Position -> Int
{-# INLINE posLine #-}
posLine UnknownPos = -1 :: Int
posLine (SourcePos _ ln _ _) = ln

posStart :: Position -> Int
{-# INLINE posStart #-}
posStart UnknownPos = -1 :: Int
posStart (SourcePos _ _ st _) = st

posEnd :: Position -> Int
{-# INLINE posEnd #-}
posEnd UnknownPos = -1 :: Int
posEnd (SourcePos _ _ _ end) = end

newPosition :: String -> Position
{-# INLINE newPosition #-}
newPosition modName = SourcePos
    (Module Export (prim modName)) 0 0 0


instance Eq Var where
    (==) = (==) `on` varName

instance Ord Var where
    (<=) = (<=) `on` varName
    (>=) = (>=) `on` varName
    (<) = (<) `on` varName
    (>) = (>) `on` varName
    compare = comparing varName

instance Pretty Expr where
    pretty (ValueE v) = pretty v
    pretty (Pragma pr) = "Pragma "+|show pr
    pretty (FuncDecl pur vis name cons ts)
        = printf
        "Function Type Declaration:\n\
        \    Visibility  : %s\n\
        \    Purity      : %s\n\
        \    Name        : %s\n\
        \    Constraints : \n%s\
        \    Type        : %s\n"
        (show vis) (show pur) (pretty name)
        (indentAllUsing pretty{- -} cons)
        (", " `seps` ts)
    pretty (FuncDef name pars bdy) = printf
        "Function Definition:\n\
        \    Name       : %s\n\
        \    Parameters : \n%s\
        \    Body       : \n%s"
        (pretty name)
        (indentAllUsing pretty pars)
        (indentAllUsing pretty bdy)
    pretty (DataDef vis name tvs ctrs) = printf
        "Datatype Definition:\n\
        \    Visibility   : %s\n\
        \    Name         : %s\n\
        \    Type Vars    : \n%s\
        \    Constructors : \n%s"
        (show vis) (pretty name)
        (indentAllUsing pretty tvs)
        (indentAllUsing pretty ctrs)
    pretty (IfElse cnd tBdy fBdy) = printf
        "If Else Stmt:\n\
        \    Clause     : \n%s\
        \    True-Body  : \n%s\
        \    False-Body : \n%s"
        (indentUsing pretty cnd)
        (indentAllUsing pretty tBdy)
        (indentAllUsing pretty fBdy)
    pretty (Pattern val cases) = printf
        "Pattern Match:\n\
        \    Value : %s\n\
        \    Cases : \n%s"
        (pretty val)
        (indentAllUsing show cases)
    pretty (Loop ini con itr b) = printf
        "Loop:\n\
        \    Init Stmnt : \n%s\n\
        \    Condition  : \n%s\n\
        \    Iter Stmnt : \n%s\n\
        \    Body       : \n%s"
        (maybe "None" pretty ini)
        (pretty con)
        (maybe "None" pretty itr)
        (indentAllUsing pretty b)
    pretty (TraitDecl vis cons name tvs ms) = printf
        "Trait Declaration:\n\
        \    Visibility  : %s\n\
        \    Constraints : \n%s\
        \    Name        : %s\n\
        \    Type Var    : %s\n\
        \    Methods     : \n%s"
        (show vis) (indentAllUsing pretty{- -} cons)
        (pretty name) (pretty tvs)
        (indentAllUsing pretty ms)
    pretty (TraitImpl name cons Nothing ms) = printf
        "Trait Defaults:\n\
        \    Name        : %s\n\
        \    Constraints : \n%s\
        \    Method Defs : \n%s"
        (pretty name)
        (indentAllUsing pretty{- -} cons)
        (indentAllUsing pretty ms)
    pretty (TraitImpl name cons (Just t) ms) = printf
        "Trait Implementation:\n\
        \    Name        : %s\n\
        \    Constraints : \n%s\
        \    Type Name   : %s\n\
        \    Method Defs : \n%s"
        (pretty name)
        (indentAllUsing pretty{- -} cons)
        (pretty t)
        (indentAllUsing pretty ms)
    pretty (NewVar mut typ name val) = printf
        "New Var Definition:\n\
        \    Mutability : %s\n\
        \    Type       : %s\n\
        \    Name       : %s\n\
        \    Value      : \n%s"
        (show mut) (pretty typ) (pretty name)
        (indentUsing pretty val)
    pretty (Reassign name val) = printf
        "Var Reassignment:\n\
        \    Name  : %s\n\
        \    Value : \n%s"
        (pretty name) (indentUsing pretty val)
    pretty (Return val) = printf
        "Return: %s"
        (pretty val)

instance Pretty Ctor where
    pretty (Ctor vis name []) = printf
        "%s %s" (show vis) (pretty name)
    pretty (Ctor vis name ts) = printf
        "%s %s => %s"
        (show vis) (pretty name)
        (", " `seps` ts)

instance Pretty Constraint where
    pretty (Constraint con typ) = printf "%s %s"
        (pretty con) (pretty typ)

instance Pretty TypeDecl where
    pretty (TypeDecl cons typ) = printf
        "Type Declaration:\n\
        \    Constraints :\n%s\
        \    Type(s) : %s"
        (indentAllUsing pretty cons)
        (pretty typ)

instance Pretty Value where
    pretty (FuncCall var args) = printf
        "Function Call:\n\
        \    Name :%s\n\
        \    Arguments :\n%s"
        (pretty var) (indentAllUsing pretty args)
    pretty (StmtVal e)
        = "StmtVal: " ++ pretty e
    pretty (CtorVal name [])
        = "Nullary Ctor Call: " ++ pretty name
    pretty (CtorVal name as) = printf
        "Data Ctor Call:\n\
        \    Name   : %s\n\
        \    Params : \n%s"
        (pretty name)
        (indentAllUsing pretty as)
    pretty v = show v

instance Pretty Visibility where
    pretty Export = "export"
    pretty Intern = "intern"

instance Pretty Purity where
    pretty Pure = "pure"
    pretty Impure = "impure"
    pretty Unsafe = "unsafe"

instance Pretty Type where
    pretty (TerminalType ht []) = pretty ht
    pretty (TerminalType ht tps) = printf
        "%s %s" (pretty ht)
        (", " `seps` tps)
    pretty (NonTermType t1 ts) = printf
        "(%s, %s)" (pretty t1)
        (", " `seps` toList ts)

instance Pretty Position where
    pretty UnknownPos = "[?]"
    pretty (SourcePos _ ln st _) =
        printf "[%d,%d]" ln st
    detailed UnknownPos = "[?]"
    detailed (SourcePos _ ln st end) = printf
        "[%d,%d:%d]" ln st end
    exhaustive UnknownPos = "[?]"
    exhaustive (SourcePos name ln st _) = printf
        "in %s: line %d, col %d"
        (exhaustive name) ln st

instance Pretty Module where
    pretty UnknownMod = "Unknown"
    pretty (Module _ name) = pretty name
    detailed UnknownMod = "Unknown"
    detailed (Module Export name) = pretty name ++ "[E]"
    detailed (Module Intern name) = pretty name ++ "[I]"
    exhaustive UnknownMod = "Unknown"
    exhaustive (Module Export name) = pretty name ++ "[export]"
    exhaustive (Module Intern name) = pretty name ++ "[intern]"

instance Pretty Var where
    pretty = varName
    detailed (Var name UnknownPos) = name ++ detailed pos
    detailed (Var name pos) = name ++ detailed pos

