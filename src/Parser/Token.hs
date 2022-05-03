module Parser.Token (
    Token(..),
) where

import Common.Var
import Parser.Data
import Pretty


data Token
    = TValue Value
    | TBig Var
    | TSmall Var
    | TInfix Var
    | TPrefix Var
    | THole Value
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
    | TMut
    | TIntern
    | TExtern
    | TImport
    | TUsing
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
    pretty (TValue val) = pretty val
    pretty (TBig var) = pretty var
    pretty (TSmall var) = pretty var
    pretty (TInfix var) = pretty var
    pretty (TPrefix var) = pretty var
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
    pretty TPure = "pure"
    pretty TImpure = "impure"
    pretty TLet = "let"
    pretty TMut = "mut"
    pretty TIntern = "intern"
    pretty TExtern = "extern"
    pretty TImport = "import"
    pretty TUsing = "using"
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
    detailed (TPrefix var) = "TPrefix ("+|var|+")"
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
    detailed TPure = "TPure"
    detailed TImpure = "TImpure"
    detailed TLet = "TLet"
    detailed TMut = "TMut"
    detailed TIntern = "TIntern"
    detailed TExtern = "TExtern"
    detailed TImport = "TImport"
    detailed TUsing = "TUsing"
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
