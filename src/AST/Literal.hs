module AST.Literal (

) where


data Literal
    = IntLit {-# UNPACK #-} !Int64 SrcPos
    | FloatLit {-# UNPACK #-} !Float SrcPos
    | DoubleLit {-# UNPACK #-} !Double SrcPos
    | CharLit {-# UNPACK #-} !Char SrcPos
    | StringLit String SrcPos
