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
    | TBig Var
    | TSmall Var
    | TInfix Var
    | THole Pattern
    | TEq SrcPos
    | TColon SrcPos
    | TSemi SrcPos
    | TPipe SrcPos
    | TArrow SrcPos
    | TEqArrow SrcPos
    | TComma SrcPos
    | TLParen SrcPos
    | TRParen SrcPos
    | TLBrace SrcPos
    | TRBrace SrcPos
    | TLBracket SrcPos
    | TRBracket SrcPos
    | TLAngle SrcPos
    | TRAngle SrcPos
    | TUsing SrcPos
    | TPure SrcPos
    | TImpure SrcPos
    | TLet SrcPos
    | TMut SrcPos
    | TIntern SrcPos
    | TExport SrcPos
    | TExtern SrcPos
    | TImport SrcPos
    | TReturn SrcPos
    | TIf SrcPos
    | TThen SrcPos
    | TElse SrcPos
    | TMatch SrcPos
    | TLoop SrcPos
    | TBreak SrcPos
    | TContinue SrcPos
    | TImpl SrcPos
    | TTrait SrcPos
    | TData SrcPos
    | TEOF


instance HasSrcPos Token where
    getPos (TValue val) = getPos val
    getPos (TBig var) = getPos var
    getPos (TSmall var) = getPos var
    getPos (TInfix var) = getPos var
    getPos (THole hole) = getPos hole
    getPos (TEq p) = p
    getPos (TColon p) = p
    getPos (TSemi p) = p
    getPos (TPipe p) = p
    getPos (TArrow p) = p
    getPos (TEqArrow p) = p
    getPos (TComma p) = p
    getPos (TLParen p) = p
    getPos (TRParen p) = p
    getPos (TLBrace p) = p
    getPos (TRBrace p) = p
    getPos (TLBracket p) = p
    getPos (TRBracket p) = p
    getPos (TLAngle p) = p
    getPos (TRAngle p) = p
    getPos (TUsing p) = p
    getPos (TPure p) = p
    getPos (TImpure p) = p
    getPos (TLet p) = p
    getPos (TMut p) = p
    getPos (TIntern p) = p
    getPos (TExport p) = p
    getPos (TExtern p) = p
    getPos (TImport p) = p
    getPos (TReturn p) = p
    getPos (TIf p) = p
    getPos (TThen p) = p
    getPos (TElse p) = p
    getPos (TMatch p) = p
    getPos (TLoop p) = p
    getPos (TBreak p) = p
    getPos (TContinue p) = p
    getPos (TImpl p) = p
    getPos (TTrait p) = p
    getPos (TData p) = p
    getPos TEOF = UnknownPos

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
    terse TUsing{} = "keyword 'using'"
    terse TPure{} = "keyword 'pure'"
    terse TImpure{} = "keyword 'impure'"
    terse TLet{} = "keyword 'let'"
    terse TMut{} = "keyword 'mut'"
    terse TIntern{} = "keyword 'intern'"
    terse TExport{} = "keyword 'export'"
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
    pretty TUsing{} = "using"
    pretty TPure{} = "pure"
    pretty TImpure{} = "impure"
    pretty TLet{} = "let"
    pretty TMut{} = "mut"
    pretty TIntern{} = "intern"
    pretty TExport{} = "export"
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
    detailed TUsing{} = "TUsing"
    detailed TPure{} = "TPure"
    detailed TImpure{} = "TImpure"
    detailed TLet{} = "TLet"
    detailed TMut{} = "TMut"
    detailed TIntern{} = "TIntern"
    detailed TExport{} = "TExport"
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
