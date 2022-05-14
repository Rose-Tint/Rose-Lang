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
    = Param Var
    | Hole SrcPos
    | CtorPtrn Var [Pattern]
    | TuplePtrn [Pattern]
    | LitPtrn Literal
    | OrPtrn Pattern Pattern


-- | calculates the generality of a pattern by
-- counting how many wildcards there are
generality :: Pattern -> Specificity
generality (CtorPtrn _ ps) = sum (generality <$> ps)
generality (TuplePtrn ps) = sum (generality <$> ps)
generality LitPtrn{} = 0
generality (OrPtrn p1 p2) = generality p1 + generality p2
generality _ = 1


instance HasSrcPos Pattern where
    getPos (Param name) = getPos name
    getPos (Hole p) = p
    getPos (CtorPtrn name _) = getPos name
    getPos (TuplePtrn p) = getPos (head p)
    getPos (LitPtrn lit) = getPos lit
    getPos (OrPtrn p _) = getPos p

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
