module Parser.Token (
    Token(..),
) where

import AST.Literal
import AST.Pattern
import Common.SrcPos
import Common.Var
import Text.Pretty


data Token
    = TValue Literal
    | TBig {-# UNPACK #-} !Var
    | TSmall {-# UNPACK #-} !Var
    | TInfix {-# UNPACK #-} !Var
    | THole Pattern
    | TEq
    | TColon
    | TSemi
    | TPipe
    | TArrow
    | TEqArrow
    | TComma
    | TLParen
    | TRParen
    | TLBrace
    | TRBrace
    | TLBracket
    | TRBracket
    | TLAngle
    | TRAngle
    | TPure
    | TImpure
    | TLet
    | TIn
    | TExtern
    | TImport
    | TReturn
    | TIf
    | TThen
    | TElse
    | TMatch
    | TLoop
    | TBreak
    | TContinue
    | TImpl
    | TTrait
    | TData
    | TEOF


instance HasSrcPos Token where
    getPos (TValue val) = getPos val
    getPos (TBig var) = getPos var
    getPos (TSmall var) = getPos var
    getPos (TInfix var) = getPos var
    getPos (THole hole) = getPos hole
    getPos _ = UnknownPos

instance Pretty Token where
    terse (TValue val) = "value ("+|val|+")"
    terse (TBig var) = "'big' variable ("+|var|+")"
    terse (TSmall var) = "'small' variable ("+|var|+")"
    terse (TInfix var) = "'infix' variable ("+|var|+")"
    -- terse (TPrefix var) = "'prefix' variable ("+|var|+")"
    terse THole{} = "hole"
    terse TEq{} = "'='"
    terse TColon{} = "':'"
    terse TSemi{} = "';'"
    terse TPipe{} = "'|'"
    terse TArrow{} = "'->'"
    terse TEqArrow{} = "'=>'"
    terse TComma{} = "','"
    terse TLParen{} = "'('"
    terse TRParen{} = "')'"
    terse TLBrace{} = "'{'"
    terse TRBrace{} = "'}'"
    terse TLBracket{} = "'['"
    terse TRBracket{} = "']'"
    terse TLAngle{} = "'<'"
    terse TRAngle{} = "'>'"
    terse TPure{} = "keyword 'pure'"
    terse TImpure{} = "keyword 'impure'"
    terse TLet{} = "keyword 'let'"
    terse TIn{} = "keyword 'in'"
    terse TExtern{} = "keyword 'extern'"
    terse TImport{} = "keyword 'import'"
    terse TReturn{} = "keyword 'return'"
    terse TIf{} = "keyword 'if'"
    terse TThen{} = "keyword 'then'"
    terse TElse{} = "keyword 'else'"
    terse TMatch{} = "keyword 'match'"
    terse TLoop{} = "keyword 'loop'"
    terse TBreak{} = "keyword 'break'"
    terse TContinue{} = "keyword 'continue'"
    terse TImpl{} = "keyword 'impl'"
    terse TTrait{} = "keyword 'trait'"
    terse TData{} = "keyword 'data'"
    terse TEOF = "EOF"
    pretty (TValue val) = pretty val
    pretty (TBig var) = pretty var
    pretty (TSmall var) = pretty var
    pretty (TInfix var) = pretty var
    -- pretty (TPrefix var) = pretty var
    pretty (THole hole) = pretty hole
    pretty TEq{} = "="
    pretty TColon{} = ":"
    pretty TSemi{} = ";"
    pretty TPipe{} = "|"
    pretty TArrow{} = "->"
    pretty TEqArrow{} = "=>"
    pretty TComma{} = ","
    pretty TLParen{} = "("
    pretty TRParen{} = ")"
    pretty TLBrace{} = "{"
    pretty TRBrace{} = "}"
    pretty TLBracket{} = "["
    pretty TRBracket{} = "]"
    pretty TLAngle{} = "<"
    pretty TRAngle{} = ">"
    pretty TPure{} = "pure"
    pretty TImpure{} = "impure"
    pretty TLet{} = "let"
    pretty TIn{} = "in"
    pretty TExtern{} = "extern"
    pretty TImport{} = "import"
    pretty TReturn{} = "return"
    pretty TIf{} = "if"
    pretty TThen{} = "then"
    pretty TElse{} = "else"
    pretty TMatch{} = "match"
    pretty TLoop{} = "loop"
    pretty TBreak{} = "break"
    pretty TContinue{} = "continue"
    pretty TImpl{} = "impl"
    pretty TTrait{} = "trait"
    pretty TData{} = "data"
    pretty TEOF = ""
    detailed (TValue val) = "TValue ("+|val|+")"
    detailed (TBig var) = "TBig ("+|var|+")"
    detailed (TSmall var) = "TSmall ("+|var|+")"
    detailed (TInfix var) = "TInfix ("+|var|+")"
    -- detailed (TPrefix var) = "TPrefix ("+|var|+")"
    detailed (THole hole) = "THole ("+|hole|+")"
    detailed TEq{} = "TEq"
    detailed TColon{} = "TColon"
    detailed TSemi{} = "TSemi"
    detailed TPipe{} = "TPipe"
    detailed TArrow{} = "TArrow"
    detailed TEqArrow{} = "TEqArrow"
    detailed TComma{} = "TComma"
    detailed TLParen{} = "TLParen"
    detailed TRParen{} = "TRParen"
    detailed TLBrace{} = "TLBrace"
    detailed TRBrace{} = "TRBrace"
    detailed TLBracket{} = "TLBracket"
    detailed TRBracket{} = "TRBracket"
    detailed TLAngle{} = "TLAngle"
    detailed TRAngle{} = "TRAngle"
    detailed TPure{} = "TPure"
    detailed TImpure{} = "TImpure"
    detailed TLet{} = "TLet"
    detailed TIn{} = "TIn"
    detailed TExtern{} = "TExtern"
    detailed TImport{} = "TImport"
    detailed TReturn{} = "TReturn"
    detailed TIf{} = "TIf"
    detailed TThen{} = "TThen"
    detailed TElse{} = "TElse"
    detailed TMatch{} = "TMatch"
    detailed TLoop{} = "TLoop"
    detailed TBreak{} = "TBreak"
    detailed TContinue{} = "TContinue"
    detailed TImpl{} = "TImpl"
    detailed TTrait{} = "TTrait"
    detailed TData{} = "TData"
    detailed TEOF = "TEOF"
