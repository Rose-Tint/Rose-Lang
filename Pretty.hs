{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types        #-}

module Pretty where

import Control.Monad ((<$!>))
import Data.List (intercalate)
import System.IO (Handle, hPutStrLn)
import Text.Printf(IsChar(..))


default (Int, Double)



class Pretty a where
    pretty :: a -> String
    -- |Like pretty, but a more detailed version. Defaults
    -- to pretty
    detailed :: a -> String
    detailed = pretty
    -- |Like detailed, but even more detailed. Defaults
    -- to detailed
    exhaustive :: a -> String
    exhaustive = detailed
    {-# INLINABLE pretty #-}
    default pretty :: (Show a) => a -> String
    pretty = show



seps, sepsD, sepsE ::
    (Pretty a) => String -> [a] -> String
seps sep as = intercalate sep (pretty <$!> as)
sepsD sep as = intercalate sep (detailed <$!> as)
sepsE sep as = intercalate sep (exhaustive <$!> as)


prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = putStrLn . pretty


hPrettyPrint :: (Pretty a) => Handle -> a -> IO ()
hPrettyPrint hdl = hPutStrLn hdl . pretty



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
        (indentAllUsing pretty cons)
        (intercalate ", " (pretty <$!> ts))
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
        (show vis) (indentAllUsing pretty cons)
        (pretty name) (pretty tvs)
        (indentAllUsing pretty ms)
    pretty (TraitImpl name cons Nothing ms) = printf
        "Trait Defaults:\n\
        \    Name        : %s\n\
        \    Constraints : \n%s\
        \    Method Defs : \n%s"
        (pretty name)
        (indentAllUsing pretty cons)
        (indentAllUsing pretty ms)
    pretty (TraitImpl name cons (Just t) ms) = printf
        "Trait Implementation:\n\
        \    Name        : %s\n\
        \    Constraints : \n%s\
        \    Type Name   : %s\n\
        \    Method Defs : \n%s"
        (pretty name)
        (indentAllUsing pretty cons)
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
        (intercalate ", " (pretty <$!> ts))


instance Pretty Constraint where
    pretty (Constraint con typ) = printf "{ %s (%s) }"
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


instance Pretty Type where
    pretty (TerminalType ht []) = pretty ht
    pretty (TerminalType ht tps) = printf
        "%s %s" (pretty ht)
        (intercalate ", " (pretty <$!> tps))
    pretty (NonTermType t1 ts) = printf
        "(%s, %s)" (pretty t1)
        (intercalate ", " (pretty <$!> toList ts))


instance Pretty Variable where
    pretty (Var name ln st) =
        printf "%s<%d:%d>" name ln st
    pretty (Prim name) = name

instance (IsChar c) => Pretty [c] where
    pretty cs = fmap toChar cs
