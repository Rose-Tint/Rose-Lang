{
module Front.Parser.Parser (rose) where

import Data.Array (listArray)

import Common.Item
import Common.Typing
import Common.Var
import Front.Parser.Data
import Front.Parser.Imports
import Front.Parser.Lexer
import Pretty
}

{- TODO:
 - FFI
 - seperate imports from top-level expressions
 -}

%name rose Module
%error { parseError }
%lexer { lexer } { TEOF }
%monad { Alex }

%tokentype { Token }
%token
    -- keywords
    using           { TUsing      }
    pure            { TPure       }
    impure          { TImpure     }
    let             { TLet        }
    mut             { TMut        }
    intern          { TIntern     }
    export          { TExport     }
    extern          { TExtern     }
    import          { TImport     }
    return          { TReturn     }
    if              { TIf         }
    else            { TElse       }
    match           { TMatch      }
    loop            { TLoop       }
    break           { TBreak      }
    continue        { TContinue   }
    impl            { TImpl       }
    trait           { TTrait      }
    data            { TData       }
    -- reserved symbols
    "="             { TEq         }
    ":"             { TColon      }
    ";"             { TSemi       }
    "|"             { TPipe       }
    "->"            { TArrow      }
    "=>"            { TEqArrow    }
    ","             { TComma      }
    -- groupers
    "("             { TLParen     }
    ")"             { TRParen     }
    "{"             { TLBrace     }
    "}"             { TRBrace     }
    "["             { TLBracket   }
    "]"             { TRBracket   }
    "<"             { TLAngle     }
    ">"             { TRAngle     }
    -- literals
    literal         { TValue $$   }
    -- identifiers
    big_id          { TBig $$     }
    small_id        { TSmall $$   }
    infix_id        { TInfix $$   }
    "_"             { THole $$    }

%nonassoc infix_id
%left big_id small_id
%nonassoc literal
%right if else "=>" "(" "[" ";"

-- ghost precedence for function application
-- https://stackoverflow.com/questions/27630269
%nonassoc APP


%%

Module :: { Module }
    : Imports0 TopLevelExprs0  { Module (reverse $1) (reverse $2) }

Imports0 :: { [Import] }
    : {- empty -}       { [] }
    | Imports0 Import   { ($2:$1) }

Import :: { Import }
    : import Vis big_id { Import $3 $2 }

TopLevelExprs0 :: { [Expr] }
    : {- empty -}                   { [] }
    | TopLevelExprs0 TopLevelExpr    { ($2:$1) }

TopLevelExpr :: { Expr }
    : FuncDecl  { $1 }
    | FuncDef   { $1 }
    | DataDef   { $1 }
    | TypeAlias { $1 }
    | TraitDecl { $1 }
    | TraitImpl { $1 }

FuncDecl :: { Expr }
    : Pur Vis small_id TypeDecl            { FuncDecl $1 $2 $3 $4 }
    | Pur Vis "(" infix_id ")" TypeDecl    { FuncDecl $1 $2 $4 $6 }
    | extern Pur Vis small_id TypeDecl     { FuncDecl $2 $3 $4 $5 }

Pur :: { Purity }
    : pure      { Pure }
    | impure    { Impure }

Vis :: { Visib }
    : {- empty -}   { Export }
    | export        { Export }
    | intern        { Intern }

TypeDecl :: { TypeDecl }
    : "<" CtxSeq ArrowSepTypes1 ">" { TypeDecl $2 (Applied $3) }
    | "<" ArrowSepTypes1 ">" { TypeDecl [] (Applied $2) }

CtxSeq :: { Context }
    : CtxSeq_ ":"   { reverse $1 }

CtxSeq_ :: { Context }
    : CtxSeq_ "," Constraint  { ($3:$1) }
    | Constraint              { [$1]    }

Constraint :: { Constraint }
    : big_id SmallIds0   { Constraint $1 $2 }

ArrowSepTypes1 :: { [Type] }
    : ArrowSepTypes1_    { reverse $1 }

ArrowSepTypes1_ :: { [Type] }
    : Type                      { [$1]      }
    | ArrowSepTypes1_ "->" Type { ($3:$1) }

Type :: { Type }
    : big_id Types0             { Type $1 $2 }
    | small_id Types0           { Param $1 $2 }
    | "[" Type "]"              { Type (prim "[]") [$2] }
    | "(" CommaSepTypes2 ")"    { Type (prim ",") $2 }
    | "(" ArrowSepTypes1 ")"    { Applied $2 }

Types0 :: { [Type] }
    : Types1_        { reverse $1 }
    | {- empty -}    { [] }
-- BUG?? moving the empty rule from `Types1_`
-- (formerly `Types0_`) fixed an issue with
-- the first two rules of `Type` not allowing
-- "->" to follow when there was more than one
-- type-parameter (see `Type`, rules 1 & 2),
-- but added ~15 s/r conflicts
Types1_ :: { [Type] }
    : Type           { [$1] }
    | Types1_ Type   { ($2:$1) }

CommaSepTypes2 :: { [Type] }
    : CommaSepTypes2_    { reverse $1 }

CommaSepTypes2_ :: { [Type] }
    : CommaSepTypes2_ "," Type { ($3:$1) }
    | Type "," Type           { [$3, $1] }

FuncDef :: { Expr }
    : Pattern infix_id Pattern BodyAssn { FuncDef $2 [$1,$3] $4 }
    | small_id Params0 BodyAssn         { FuncDef $1 $2 $3 }
    | "(" infix_id ")" Params0 BodyAssn { FuncDef $2 $4 $5 }

Params0 :: { [Value] }
    : Params0_  { reverse $1 }

Params0_ :: { [Value] }
    : {- empty -}                { [] }
    | Params0_ Pattern  { ($2:$1) }

BodyAssn :: { Body }
    : "=" Stmt   { [Return (StmtVal $2)] }
    | Body       { $1   }

-- TODO: labeled loops
Stmt :: { Stmt }
    : IfElse            { $1 }
    | Match             { $1 }
    | break ";"         { Break }
    | continue ";"      { Continue }
    | return Term ";"   { Return $2 }
    | Expr              { $1 }
    | Loop              { $1 }

IfElse :: { Stmt }
    : if Term StmtBody else StmtBody    { IfElse $2 $3 $5 }
    | if Term StmtBody                  { IfElse $2 $3 [] }

Term :: { Value }
    : literal               { $1 }
    | "[" ArrayTerms1 "]"   { mkArray $2 }
    | "(" TupleTerms2 ")"   { mkTuple $2 }
    | SmallIds1             {
        let (var:vars) = fmap VarVal $1
        in case vars of
                [] -> var
                _ -> Application var vars
        }
    | Lambda                { $1 }
    | FuncCall              { $1 }
    | "(" Term ")"          { $2 }

ArrayTerms1 :: { [Value] }
    : ArrayTerms1 "," Term   { ($3:$1) }
    | ArrayTerms1 ","        { $1      }
    | Term                  { [$1]    }

TupleTerms2 :: { [Value] }
    : TupleTerms2 "," Term   { ($3:$1) }
    | Term "," Term         { [$1] }

CtorCall :: { Value }
    : big_id Terms0 { CtorCall $1 $2 }

StmtBody :: { Body }
    : Stmt  { [$1] }
    | Body  { $1   }

Match :: { Stmt }
    : match Term "{" Cases1 "}" { Match $2 $4 }

Cases1 :: { [MatchCase] }
    : Cases1 Pattern BodyAssn   { (($2,$3):$1) }
    | Pattern BodyAssn          { [($1,$2)] }

Pattern :: { Value }
    : "_"                   { $1 }
    | small_id              { VarVal $1 }
    | "[" PatternItem "]"   { $2 }

PatternItem :: { Value }
    : literal       { $1 }
    | TuplePattern  { $1 }
    | CtorPattern   { $1 }

TuplePattern :: { Value }
    : "(" TuplePtrns2_ ")" { mkTuple (reverse $2) }

TuplePtrns2_ :: { [Value] }
    : TuplePtrns2_ "," Pattern   { ($3:$1)  }
    | Pattern "," Pattern      { [$3, $1] }

CtorPattern :: { Value }
    : big_id Patterns0   { CtorCall $1 $2 }

Patterns0 :: { [Value] }
    : Patterns0_ { reverse $1 }

Patterns0_ :: { [Value] }
    : {- empty -}       { [] }
    | Patterns0 Pattern  { ($2:$1) }

Expr :: { Stmt }
    : NewVar                { $1 }
    | small_id "=" Term ";" { Reassignment $1 $3 }
    | Term ";"          { ValStmt $1 }
    | ";"                   { NullStmt }

NewVar :: { Stmt }
    : let small_id VarType "=" Term ";"        { NewVar Mut $2 $3 $5 }
    | let mut small_id VarType "=" Term ";"    { NewVar Imut $3 $4 $6 }

VarType :: { TypeDecl }
    : TypeDecl      { $1 }
    | {- empty -}   { TypeDecl [] delayed }

FuncCall :: { Value }
    : Term infix_id Term    { Application (VarVal $2) [$1, $3] }
    | infix_id              { VarVal $1 }
    | CtorCall              { $1 }
    | Term Term %prec APP   { Application $1 [$2] }

Terms0 :: { [Value] }
    : Terms0_    { reverse $1 }

Terms0_ :: { [Value] }
    : {- empty -}   { []      }
    | Terms0_ Term    { ($2:$1) }

Lambda :: { Value }
    : SmallIds1 "=>" Body    { Lambda $1 $3 }
    | SmallIds1 "=>" Term    { Lambda $1 [Return $3] }

SmallIds1 :: { [Var] }
    : SmallIds1_ %prec APP { reverse $1 }

SmallIds1_ :: { [Var] }
    : SmallIds1_ small_id    { ($2:$1) }
    | small_id               { [$1] }

SmallIds0 :: { [Var] }
    : SmallIds1_    { reverse $1 }
    | {- empty -}   { [] }

Loop :: { Stmt }
    : loop "(" Expr Expr Expr ")" StmtBody  { Loop $3 $4 $5 $7 }
    | loop Term StmtBody                    { whileLoop $2 $3 }
    | loop Body                             { foreverLoop $2 }

Body :: { Body }
    : "{" Stmts0 "}"    { $2 }

Stmts0 :: { [Stmt] }
    : Stmts0_    { reverse $1 }

Stmts0_ :: { [Stmt] }
    : {- empty -}   { []      }
    | Stmts0_ Stmt  { ($2:$1) }

DataDef :: { Expr }
    : data Vis big_id SmallIds0 CtorList    { DataDef $2 $3 $4 $5 }

CtorList :: { [Ctor] }
    : "=" CtorDef           { [$2] }
    | CtorList "|" CtorDef  { ($3:$1) }

CtorDef :: { Ctor }
    : Vis big_id                        { SumType $2 $1 [] }
    | Vis big_id "<" ArrowSepTypes1 ">" { SumType $2 $1 $4 }

TypeAlias :: { Expr }
    : using Vis big_id "=" Type  { TypeAlias $2 $3 $5 }

TraitDecl :: { Expr }
    : trait Vis TraitCtx big_id SmallIds1 "{" MethodDecls1 "}"    { TraitDecl $2 $3 $4 $5 $7 }

TraitCtx :: { Context }
    : {- empty -}       { [] }
    | "<" CtxSeq ">"    { $2 }

MethodDecls1 :: { [Expr] }
    : FuncDecl                { [$1] }
    | MethodDecls1 FuncDecl   { ($2:$1) }

TraitImpl :: { Expr }
    : impl TraitCtx big_id Types0 "{" MethodImpls0 "}"    { TraitImpl $2 $3 $4 $6 }

MethodImpls0 :: { [Expr] }
    : {- empty -}            { []      }
    | MethodImpls0 FuncDef    { ($2:$1) }



{- FOREIGN FUNCTION INTERFACE

ForeignFunc :: { () }
    : extern Pur small_id



-}

{

parseError :: Token -> Alex a
parseError = lexError . terse

whileLoop :: Value -> Body -> Stmt
whileLoop stmt = Loop NullStmt (ValStmt stmt) NullStmt

foreverLoop :: Body -> Stmt
foreverLoop  = Loop NullStmt NullStmt NullStmt

mkRevValArray :: [Value] -> ValArray
mkRevValArray vals = listArray
    -- without the `- 1`, i get an error saying
    -- "(Array.!): undefined array element". idk.
    (0, length vals - 1)
    (reverse vals)

mkTuple :: [Value] -> Value
mkTuple = Tuple . mkRevValArray

mkArray :: [Value] -> Value
mkArray = Array . mkRevValArray

}
