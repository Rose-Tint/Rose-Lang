module AST.Pattern (
    Pattern(..),
    generality,
) where

import AST.Literal
import Common.SrcPos
import Common.Var
import Text.Pretty


type Specificity = Int

data Pattern
    = Param {-# UNPACK #-} !Var
    | Hole SrcPos
    | CtorPtrn Var [Pattern]
    | TuplePtrn [Pattern]
    | LitPtrn Literal


-- | calculates the generality of a pattern by
-- counting how many wildcards there are
generality :: Pattern -> Specificity
generality (CtorPtrn _ ps) = sum (generality <$> ps)
generality (TuplePtrn ps) = sum (generality <$> ps)
generality LitPtrn{} = 0
generality _ = 1


instance HasSrcPos Pattern where
    getPos (Param name) = getPos name
    getPos (Hole p) = p
    getPos (CtorPtrn name _) = getPos name
    getPos (TuplePtrn p) = getPos (head p)
    getPos (LitPtrn lit) = getPos lit

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
prettyHelper ptrn = pretty ptrn
