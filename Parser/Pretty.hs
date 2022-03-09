module Parser.Pretty where

import Data.List (intercalate)
import Text.Printf (printf)

import Utils
import Parser.Data (
    Expr(..),
    Value(..),
    DataCtor(..),
    Type(..),
    Typename(..),
    Variable(..),
    )



prettyExpr :: Expr -> String
prettyExpr (ValueE v) = show v
prettyExpr (ModImport vis name) = printf
    "Module Import:\n\
    \    Visibility : %s\n\
    \    Name       : %s"
    (show vis) (varName name)
prettyExpr (FuncTypeDecl pur vis name cons ts)
    = printf
    "Function Type Declaration:\n\
    \    Visibility  : %s\n\
    \    Purity      : %s\n\
    \    Name        : %s\n\
    \    Constraints : \n%s\
    \    Type        : %s\n"
    (show vis) (show pur) (varName name)
    (indentAllUsing show cons)
    (prettyTypes ts)
prettyExpr (FuncDef name pars bdy) = printf
    "Function Definition:\n\
    \    Name       : %s\n\
    \    Parameters : \n%s\
    \    Body       : \n%s"
    (varName name)
    (indentAllUsing prettyValue pars)
    (indentAllUsing prettyExpr bdy)
prettyExpr (FuncCall name args) = printf
    "Function Call:\n\
    \    Name      : %s\n\
    \    Arguments : \n%s"
    (varName name)
    (indentAllUsing prettyValue args)
prettyExpr (DataDef vis name tvs ctrs) = printf
    "Datatype Definition:\n\
    \    Visibility   : %s\n\
    \    Name         : %s\n\
    \    Type Vars    : \n%s\
    \    Constructors : \n%s"
    (show vis) (varName name)
    (indentAllUsing show tvs)
    (indentAllUsing prettyCtor ctrs)
prettyExpr (IfElse cnd tBdy fBdy) = printf
    "If Else Statement:\n\
    \    Clause     : \n%s\
    \    True-Body  : \n%s\
    \    False-Body : \n%s"
    (indentUsing prettyValue cnd)
    (indentAllUsing prettyExpr tBdy)
    (indentAllUsing prettyExpr fBdy)
prettyExpr (Pattern val cases) = printf
    "Pattern Match:\n\
    \    Value : %s\n\
    \    Cases : \n%s"
    (prettyValue val)
    (indentAllUsing show cases)
prettyExpr (Loop ini con itr b) = printf
    "Loop:\n\
    \    Init Stmnt : \n%s\n\
    \    Condition  : \n%s\n\
    \    Iter Stmnt : \n%s\n\
    \    Body       : \n%s"
    (maybe "None" prettyExpr ini)
    (prettyValue con)
    (maybe "None" prettyExpr itr)
    (indentAllUsing prettyExpr b)
prettyExpr (TraitDecl vis cons name tvs ms) = printf
    "Trait Declaration:\n\
    \    Visibility  : %s\n\
    \    Constraints : \n%s\
    \    Name        : %s\n\
    \    Type Var    : %s\n\
    \    Methods     : \n%s"
    (show vis) (indentAllUsing show cons)
    (varName name) (varName tvs)
    (indentAllUsing prettyExpr ms)
prettyExpr (TraitImpl name cons Nothing ms) = printf
    "Trait Defaults:\n\
    \    Name        : %s\n\
    \    Constraints : \n%s\
    \    Method Defs : \n%s"
    (varName name) 
    (indentAllUsing show cons)
    (indentAllUsing prettyExpr ms)
prettyExpr (TraitImpl name cons (Just t) ms) = printf
    "Trait Implementation:\n\
    \    Name        : %s\n\
    \    Constraints : \n%s\
    \    Type Name   : %s\n\
    \    Method Defs : \n%s"
    (varName name) 
    (indentAllUsing show cons)
    (prettyType t)
    (indentAllUsing prettyExpr ms)
prettyExpr (NewVar mut typ name val) = printf
    "New Variable Definition:\n\
    \    Mutability : %s\n\
    \    Type       : %s\n\
    \    Name       : %s\n\
    \    Value      : \n%s"
    (show mut) (prettyType typ) (varName name)
    (indentUsing prettyValue val)
prettyExpr (Reassign name val) = printf
    "Variable Reassignment:\n\
    \    Name  : %s\n\
    \    Value : \n%s"
    (varName name) (indentUsing prettyValue val)
prettyExpr (Return val) = printf
    "Return: %s"
    (prettyValue val)


prettyCtor :: DataCtor -> String
prettyCtor (DataCtor vis name []) = printf
    "%s %s" (show vis) (varName name)
prettyCtor (DataCtor vis name ts) = printf
    "%s %s => %s"
    (show vis) (varName name) (prettyTypes ts)


prettyValue :: Value -> String
prettyValue (ExprVal e)
    = "ExprVal: " ++ prettyExpr e
prettyValue (CtorVal name [])
    = "Nullary Ctor Call: " ++ varName name
prettyValue (CtorVal name as) = printf
    "Data Ctor Call:\n\
    \    Name   : %s\n\
    \    Params : \n%s"
    (varName name)
    (indentAllUsing prettyValue as)
prettyValue v = show v


prettyType :: Type -> String
prettyType (TerminalType ht []) = prettyTypename ht
prettyType (TerminalType ht tps) = printf
    "%s %s" (prettyTypename ht) (prettyTypes tps)
prettyType (NonTermType ts) = printf
    "(%s)" (prettyTypes ts)


prettyTypename :: Typename -> String
prettyTypename (RealType name) = varName name
prettyTypename (TypeParam name) = varName name


prettyTypes :: [Type] -> String
prettyTypes ts = intercalate ", " (fmap prettyType ts)
