module Parser.Data where



type Variable = String


type Body = [Expr]


type Constraint = (Variable, Variable)


-- Examples:
-- - `TerminalType "Either" [TerminalType "String"
--       [], TerminalType "a" []]` corresponds to
--       a Rose signature of `=> Either String a`
-- - `NonTermType [TerminalType "a" [],
--       TermainalType "b" []]` corresponds to a Rose signature
--       of `=> (a, b)`
data Type
    = NonTermType [Type]
    | TerminalType String [Type]
    deriving (Show, Eq, Ord)


data Value
    = IntLit Integer
    | FltLit Double
    | ChrLit Char
    | StrLit String
    | VarVal Variable
    | ExprVal Expr
    | CtorVal String [Value]
    | Array Int [Value]
    deriving (Show, Eq, Ord)


data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq, Ord)


data Visibility = Export | Intern
    deriving (Show, Eq, Ord)


data Mutability = Immutable | Mutable
    deriving (Show, Eq, Ord)


data DataCtor = DataCtor {
        ctorVisib :: Visibility,
        ctorName :: String,
        ctorTypes :: [Type]
    }
    deriving (Show, Eq, Ord)


data Expr
    = ValueE Value
    | ModImport {
        modImpVisib :: Visibility,
        modImpName :: String
    }
    | FuncTypeDecl {
        eFTDPurity :: Purity,
        eFTDVisib :: Visibility,
        eFTDName :: String,
        eFDCons :: [Constraint],
        eFTDType :: [Type]
    }
    | FuncDef {
        eFDName :: String,
        eFDPars :: [Value],
        eFDBody :: Body
    }
    | DataDef {
        eDDVisib :: Visibility,
        eDDName :: String,
        eDDTypePars :: [Variable],
        eDDCtors :: [DataCtor]
    }
    | IfElse {
        eIEEClause :: Value,
        eIUETrue :: Body,
        eIUEFalse :: Body
    }
    | Pattern {
        ptrnValue :: Value,
        -- |@`ptrnCases`@ is a list of tuples
        -- with a list of values to be matched
        -- against and a body. it is a list of
        -- values because it can contain multiple
        -- nullary constructors
        ptrnCases :: [([Value], Body)]
    }
    | Loop {
        loopInit :: Maybe Expr,
        loopCond :: Value,
        loopIter :: Maybe Expr,
        loopBody :: Body
    }
    | TraitDecl {
        traitVisib :: Visibility,
        traitCons :: [Constraint],
        traitName :: String,
        traitTypeVar :: Variable,
        traitFuncs :: [Expr]
    }
    | TraitImpl {
        traitName :: String,
        traitType :: Maybe Type,
        traitDefs :: [Expr]
    }
    | FuncCall String [Value]
    | NewVar Mutability Type Variable Value
    | Reassign Variable Value
    | Return Value
    deriving (Show, Eq, Ord)
