module Parser.Token (
    Token(..),
) where

import AST.Literal
import AST.Pattern
import Common.Var
import Text.Pretty


data Token
    = TValue Literal
    | TBig Var
    | TSmall Var
    | TInfix Var
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
    | TUsing
    | TPure
    | TImpure
    | TLet
    | TMut
    | TIntern
    | TExport
    | TExtern
    | TImport
    | TReturn
    | TIf
    | TElse
    | TMatch
    | TLoop
    | TBreak
    | TContinue
    | TImpl
    | TTrait
    | TData
    | TEOF


instance Pretty Token where
    terse (TValue val) = "value ("+|val|+")"
    terse (TBig var) = "'big' variable ("+|var|+")"
    terse (TSmall var) = "'small' variable ("+|var|+")"
    terse (TInfix var) = "'infix' variable ("+|var|+")"
    -- terse (TPrefix var) = "'prefix' variable ("+|var|+")"
    terse (THole _) = "hole"
    terse TEq = "'='"
    terse TColon = "':'"
    terse TSemi = "';'"
    terse TPipe = "'|'"
    terse TArrow = "'->'"
    terse TEqArrow = "'=>'"
    terse TComma = "','"
    terse TLParen = "'('"
    terse TRParen = "')'"
    terse TLBrace = "'{'"
    terse TRBrace = "'}'"
    terse TLBracket = "'['"
    terse TRBracket = "']'"
    terse TLAngle = "'<'"
    terse TRAngle = "'>'"
    terse TUsing = "keyword 'using'"
    terse TPure = "keyword 'pure'"
    terse TImpure = "keyword 'impure'"
    terse TLet = "keyword 'let'"
    terse TMut = "keyword 'mut'"
    terse TIntern = "keyword 'intern'"
    terse TExport = "keyword 'export'"
    terse TExtern = "keyword 'extern'"
    terse TImport = "keyword 'import'"
    terse TReturn = "keyword 'return'"
    terse TIf = "keyword 'if'"
    terse TElse = "keyword 'else'"
    terse TMatch = "keyword 'match'"
    terse TLoop = "keyword 'loop'"
    terse TBreak = "keyword 'break'"
    terse TContinue = "keyword 'continue'"
    terse TImpl = "keyword 'impl'"
    terse TTrait = "keyword 'trait'"
    terse TData = "keyword 'data'"
    terse TEOF = "EOF"
    pretty (TValue val) = pretty val
    pretty (TBig var) = pretty var
    pretty (TSmall var) = pretty var
    pretty (TInfix var) = pretty var
    -- pretty (TPrefix var) = pretty var
    pretty (THole hole) = pretty hole
    pretty TEq = "="
    pretty TColon = ":"
    pretty TSemi = ";"
    pretty TPipe = "|"
    pretty TArrow = "->"
    pretty TEqArrow = "=>"
    pretty TComma = ","
    pretty TLParen = "("
    pretty TRParen = ")"
    pretty TLBrace = "{"
    pretty TRBrace = "}"
    pretty TLBracket = "["
    pretty TRBracket = "]"
    pretty TLAngle = "<"
    pretty TRAngle = ">"
    pretty TUsing = "using"
    pretty TPure = "pure"
    pretty TImpure = "impure"
    pretty TLet = "let"
    pretty TMut = "mut"
    pretty TIntern = "intern"
    pretty TExport = "export"
    pretty TExtern = "extern"
    pretty TImport = "import"
    pretty TReturn = "return"
    pretty TIf = "if"
    pretty TElse = "else"
    pretty TMatch = "match"
    pretty TLoop = "loop"
    pretty TBreak = "break"
    pretty TContinue = "continue"
    pretty TImpl = "impl"
    pretty TTrait = "trait"
    pretty TData = "data"
    pretty TEOF = ""
    detailed (TValue val) = "TValue ("+|val|+")"
    detailed (TBig var) = "TBig ("+|var|+")"
    detailed (TSmall var) = "TSmall ("+|var|+")"
    detailed (TInfix var) = "TInfix ("+|var|+")"
    -- detailed (TPrefix var) = "TPrefix ("+|var|+")"
    detailed (THole hole) = "THole ("+|hole|+")"
    detailed TEq = "TEq"
    detailed TColon = "TColon"
    detailed TSemi = "TSemi"
    detailed TPipe = "TPipe"
    detailed TArrow = "TArrow"
    detailed TEqArrow = "TEqArrow"
    detailed TComma = "TComma"
    detailed TLParen = "TLParen"
    detailed TRParen = "TRParen"
    detailed TLBrace = "TLBrace"
    detailed TRBrace = "TRBrace"
    detailed TLBracket = "TLBracket"
    detailed TRBracket = "TRBracket"
    detailed TLAngle = "TLAngle"
    detailed TRAngle = "TRAngle"
    detailed TUsing = "TUsing"
    detailed TPure = "TPure"
    detailed TImpure = "TImpure"
    detailed TLet = "TLet"
    detailed TMut = "TMut"
    detailed TIntern = "TIntern"
    detailed TExport = "TExport"
    detailed TExtern = "TExtern"
    detailed TImport = "TImport"
    detailed TReturn = "TReturn"
    detailed TIf = "TIf"
    detailed TElse = "TElse"
    detailed TMatch = "TMatch"
    detailed TLoop = "TLoop"
    detailed TBreak = "TBreak"
    detailed TContinue = "TContinue"
    detailed TImpl = "TImpl"
    detailed TTrait = "TTrait"
    detailed TData = "TData"
    detailed TEOF = "TEOF"
