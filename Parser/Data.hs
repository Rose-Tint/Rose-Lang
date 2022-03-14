module Parser.Data where

import Data.List.NonEmpty (NonEmpty)

import Text.Parsec.Pos



data Variable
    = Var {
        varName :: String,
        varLine :: Line,
        varStart :: Column
    }
    | Prim {
        varName :: String
    }
    deriving (Show)


-- short term solution to type variables
-- not matching in semantic analysis
data Typename
    = RealType Variable
    | TypeParam Variable
    deriving (Show, Ord)


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
    | TerminalType Typename [Type]
    deriving (Show, Eq, Ord)


data Value
    = IntLit Integer
    | FltLit Double
    | ChrLit Char
    | StrLit String
    | FuncCall Variable [Value]
    | CtorVal Variable [Value]
    | Array Int [Value]
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
boolType = TerminalType (RealType (Prim "Boolean")) []



instance Eq Typename where
    TypeParam _ == _ = True
    _ == TypeParam _ = True
    RealType n1 == RealType n2 = n1 == n2


instance Eq Variable where
    v1 == v2 = varName v1 == varName v2


instance Ord Variable where
    v1 <= v2 = varName v1 <= varName v2
    v1 >= v2 = varName v1 >= varName v2
    v1 < v2 = varName v1 < varName v2
    v1 > v2 = varName v1 > varName v2


-- instance Eq Type where
--     TerminalType n1 ts1 == TerminalType n2 ts2 =
--         n1 == n2 && ts1 == ts2
--         where
--             cmp [] [] = False -- should not happen
--             cmp (c1:cs1) (c2:cs2) = 
--     NonTermType [] == NonTermType [] = True
--     NonTermType [t1] == NonTermType [t2] = t1 == t2
--     NonTermType [t1] == t2 = t1 == t2
--     t1 == NonTermType [t2] = t1 == t2
--     NonTermType ts1 == NonTermType ts2 = ts1 == ts2
--     _ == _ = False
