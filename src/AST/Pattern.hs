module AST.Pattern (
    Pattern(..),
) where

import AST.Literal
import Common.SrcPos
import Common.Var
import Text.Pretty


data Pattern
    = Param Var
    | Hole SrcPos
    | CtorPtrn Var [Pattern]
    | TuplePtrn [Pattern]
    | LitPtrn Literal
    | OrPtrn Pattern Pattern

instance Pretty Pattern where
    pretty (Param name) = pretty name
    pretty (Hole _) = "_"
    pretty ptrn = "["+|prettyHelper ptrn|+"]"

prettyHelper :: Pattern -> String
prettyHelper (CtorPtrn name []) = pretty name
prettyHelper (CtorPtrn name args) =
    name|+" "+|" "`seps`args
prettyHelper (TuplePtrn ptrns) =
    "("+|", "`seps`ptrns|+")"
prettyHelper (LitPtrn lit) = pretty lit
prettyHelper (OrPtrn p1 p2) = p1|+", "+|p2
prettyHelper ptrn = pretty ptrn
