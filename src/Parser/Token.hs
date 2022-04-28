module Parser.Token (
    Token(..),
) where

import Common.Var
import Parser.Data
import Pretty


data Token
    = TLiteral Value
    | TBig Var
    | TSmall Var
    | TPrefix Var
    | TInfix Var
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
    | TModule
    | TWhere
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
    pretty (TLiteral val) = pretty val
    pretty (TBig var) = pretty var
    pretty (TSmall var) = pretty var
    pretty (TPrefix var) = pretty var
    pretty (TInfix var) = pretty var
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
    pretty TModule = "module"
    pretty TWhere = "where"
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
