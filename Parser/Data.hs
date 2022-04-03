module Parser.Data where

import Data.List.NonEmpty (NonEmpty, toList)
import Text.Parsec.Pos

import Color
import Pretty
import Utils


default (Int, Double)


data Position
    = UnknownPos
    | SourcePos Module
        {-# UNPACK #-} !Line
        {-# UNPACK #-} !Column
        {-# UNPACK #-} !Column
    deriving (Show, Eq, Ord)


data Module
    = Module Visibility !Variable
    | UnknownMod
    deriving (Show, Eq, Ord)


data Variable
    = Var {
        varName :: String,
        varPos :: Position
    }
    | Prim {
        varName :: String
    }
    deriving (Show)


type Body = [Expr]


data Constraint
    = Constraint {
        consTraitName :: Variable,
        consType :: Variable
    }
    deriving (Show, Eq, Ord)


type Mutability = Purity


data Type
    = NonTermType Type (NonEmpty Type)
    | TerminalType Variable [Type]
    deriving (Show, Eq, Ord)


data Value
    = IntLit Integer Position
    | FltLit Double Position
    | ChrLit Char Position
    | StrLit String Position
    | FuncCall Variable [Value]
    | CtorVal Variable [Value]
    | Array {-# UNPACK #-} !Int [Value] Position
    | ExprVal Expr
    deriving (Show, Eq, Ord)


data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq, Ord)


data Visibility = Export | Intern
    deriving (Show, Eq, Ord)


-- data Mutability = Immutable | Mutable
--     deriving (Show, Eq, Ord)


data DataCtor = DataCtor {
        ctorVisib :: Visibility,
        ctorName :: Variable,
        ctorTypes :: [Type]
    }
    deriving (Show, Eq, Ord)


data Expr
    = ValueE Value
    | ModImport {
        exprVisib :: Visibility,
        exprName :: Variable
    }
    | FuncTypeDecl {
        exprPurity :: Purity,
        exprVisib :: Visibility,
        exprName :: Variable,
        exprCons :: [Constraint],
        exprType :: [Type]
    }
    | FuncDef {
        exprName :: Variable,
        exprPars :: [Value],
        exprBody :: Body
    }
    | DataDef {
        exprVisib :: Visibility,
        exprName :: Variable,
        exprTypePars :: [Variable],
        exprCtors :: [DataCtor]
    }
    | IfElse {
        exprClause :: Value,
        exprTrue :: Body,
        exprFalse :: Body
    }
    | Pattern {
        exprValue :: Value,
        exprCases :: [(Value, Body)]
    }
    | Loop {
        exprInit :: Maybe Expr,
        exprCond :: Value,
        exprIter :: Maybe Expr,
        exprBody :: Body
    }
    | TraitDecl {
        exprVisib :: Visibility,
        exprCons :: [Constraint],
        exprName :: Variable,
        exprTypeVar :: Variable,
        exprFuncs :: [Expr]
    }
    | TraitImpl {
        exprName :: Variable,
        exprCons :: [Constraint],
        exprTraitType :: Maybe Type,
        exprDefs :: [Expr]
    }
    | NewVar Mutability Type Variable Value
    | Reassign Variable Value
    | Return Value
    deriving (Show, Eq, Ord)



boolType :: Type
{-# INLINABLE boolType #-}
boolType = TerminalType (Prim "Boolean") []


valPos :: Value -> Position
valPos (IntLit _ p) = p
valPos (FltLit _ p) = p
valPos (ChrLit _ p) = p
valPos (StrLit _ p) = p
valPos (FuncCall var _) = varPos var
valPos (CtorVal var _) = varPos var
valPos (Array _ _ p) = p
valPos (ExprVal _) = UnknownPos


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
newPosition modName = SourcePos
    (Module Export (Prim modName)) 0 0 0



instance Eq Variable where
    v1 == v2 = varName v1 == varName v2


instance Ord Variable where
    v1 <= v2 = varName v1 <= varName v2
    v1 >= v2 = varName v1 >= varName v2
    v1 < v2 = varName v1 < varName v2
    v1 > v2 = varName v1 > varName v2


instance Pretty Expr where
    pretty (ValueE v) = pretty v
    pretty (ModImport vis name) = printf
        "Module Import:\n\
        \    Visibility : %s\n\
        \    Name       : %s"
        (show vis) (pretty name)
    pretty (FuncTypeDecl pur vis name cons ts)
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
        "If Else Statement:\n\
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
        "New Variable Definition:\n\
        \    Mutability : %s\n\
        \    Type       : %s\n\
        \    Name       : %s\n\
        \    Value      : \n%s"
        (show mut) (pretty typ) (pretty name)
        (indentUsing pretty val)
    pretty (Reassign name val) = printf
        "Variable Reassignment:\n\
        \    Name  : %s\n\
        \    Value : \n%s"
        (pretty name) (indentUsing pretty val)
    pretty (Return val) = printf
        "Return: %s"
        (pretty val)


instance Pretty DataCtor where
    pretty (DataCtor vis name []) = printf
        "%s %s" (show vis) (pretty name)
    pretty (DataCtor vis name ts) = printf
        "%s %s => %s"
        (show vis) (pretty name)
        (", " `seps` ts)


instance Pretty Constraint where
    pretty (Constraint con typ) = printf "%s %s"
        (pretty con) (pretty typ)


instance Pretty Value where
    pretty (FuncCall var args) = printf
        "Function Call:\n\
        \    Name :%s\n\
        \    Arguments :\n%s"
        (pretty var) (indentAllUsing pretty args)
    pretty (ExprVal e)
        = "ExprVal: " ++ pretty e
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
    -- exhaustive (SourcePos name ln st end) = printf
        -- "in module `%s`, line %d, col %d (ending at col %d?)"
        -- name ln st end
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


instance Pretty Variable where
    pretty = varName
    detailed (Var name pos) = name ++ detailed pos
    detailed (Prim name) = name

