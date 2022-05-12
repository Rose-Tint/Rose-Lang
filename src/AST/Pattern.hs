module AST.Pattern (

) where


data Pattern
    = Param Var
    | Hole SrcPos
    | CtorPtrn Var [Pattern]
    | TuplePtrn [Pattern]
    | LitPtrn Literal
