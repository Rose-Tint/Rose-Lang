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

%name rose Module
%error { parseError }
%lexer { lexer } { TEOF }
%monad { Alex }

-- I am almost positive about this. I will give
-- explainations as to what the conflicts are,
-- as well as why they are okay at each relevant
-- production.
%expect 15

%tokentype { Token }
%token
    -- keywords
    pure            { TPure       }
    impure          { TImpure     }
    let             { TLet        }
    mut             { TMut        }
    intern          { TIntern     }
    extern          { TExtern     }
    import          { TImport     }
    using           { TUsing      }
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
    prefix_id       { TPrefix $$  }
    "_"             { THole $$    }

%nonassoc infix_id
%left big_id small_id prefix_id
%right if else let
%nonassoc match
%right "="
%nonassoc ":"
%right "=>" "," "->" return
%left  ")" "}" "]" ">"
%right "(" "{" "[" "<" ";"



%%

Module :: { Module }
    : Imports0 TopLevelExprs  { Module $1 $2 }

Imports0 :: { [Import] }
    : {- empty -}       { [] }
    | Imports0 Import   { ($2:$1) }

Import :: { Import }
    : import big_id         { Import $2 Intern }
    | import intern big_id  { Import $3 Intern }
    | import extern big_id  { Import $3 Extern }

TopLevelExprs :: { [Expr] }
    : {- empty -}                   { [] }
    | TopLevelExprs TopLevelExpr    { ($2:$1) }

TopLevelExpr :: { Expr }
    : FuncDecl  { $1 }
    | FuncDef   { $1 }
    | DataDef   { $1 }
    | TypeAlias { $1 }
    | TraitDecl { $1 }
    | TraitImpl { $1 }

FuncDecl :: { Expr }
    : pure prefix_id TypeDecl         { FuncDecl Pure $2 $3 $4 }
    | impure prefix_id TypeDecl       { FuncDecl Impure $2 $3 $4 }
    | pure Vis prefix_id TypeDecl     { FuncDecl Pure $2 $3 $4 }
    | impure Vis prefix_id TypeDecl   { FuncDecl Impure $2 $3 $4 }

Vis :: { Visibility }
    -- : {- empty -}   { Extern }
    : extern        { Extern }
    | intern        { Intern }

TypeDecl :: { TypeDecl }
    : "<" CtxSeq ArrowSepTypes1 ">" { TypeDecl $2 (Applied $3) }

CtxSeq :: { Context }
    : {- empty -}   { [] }
    | CtxSeq_ ":" { reverse $1 }

CtxSeq_ :: { Context }
    : CtxSeq_ "," Constraint  { ($3:$1) }
    | Constraint              { [$1]    }

Constraint :: { Constraint }
    : big_id SmallIds1   { Constraint $1 $2 }

SmallIds1 :: { [Var] }
    : small_id SmallIds0     { ($1:$2) }

SmallIds0 :: { [Var] }
    : SmallIds0_ { reverse $1 }

SmallIds0_ :: { [Var] }
    : SmallIds0_ small_id    { ($2:$1) }
    | small_id               { [$1] }

ArrowSepTypes1 :: { [Type] }
    : ArrowSepTypes1_    { reverse $1 }

ArrowSepTypes1_ :: { [Type] }
    : ArrowSepTypes1_ "->" Type { ($3:$1) }
    | Type                      { [$1]      }

Type :: { Type }
    : big_id Types0             { Type $1 $2 }
    | small_id Types0           { Param $1 $2 }
    | "[" Type "]"              { Type (prim "[]") [$2] }
    | "(" CommaSepTypes2 ")"    { Type (prim ",") $2 }
    | "(" ArrowSepTypes1 ")"    { Applied $2 }

-- ALLOWED CONFLICT:
--     Does: shift according to one of:
--         "(" . CommaSepTypes2 ")"
--         "(" . ArrowSepTypes1 ")"
--         "[" . Types "]"
--         big_id . Types0
--         small_id . Types0
--     Could do: reduce using:
--         Types0 -> {- empty -}
Types0 :: { [Type] }
    : Types0_    { reverse $1 }

Types0_ :: { [Type] }
    : {- empty -}   { [] }
    | Types0_ Type   { ($2:$1) }

CommaSepTypes2 :: { [Type] }
    : CommaSepTypes2_    { reverse $1 }

CommaSepTypes2_ :: { [Type] }
    : CommaSepTypes2_ "," Type { ($3:$1) }
    | Type "," Type           { [$3, $1] }

FuncDef :: { Expr }
    : Pattern infix_id Pattern BodyAssn  { FuncDef $2 [$1,$3] $4 }
    | prefix_id PrefixParamSeq0 BodyAssn { FuncDef $1 $2 $3 }

PrefixParamSeq0 :: { [Value] }
    : PrefixParamSeq0_  { reverse $1 }

PrefixParamSeq0_ :: { [Value] }
    : {- empty -}                { [] }
    | PrefixParamSeq0_ Pattern  { ($2:$1) }

BodyAssn :: { Body }
    : "=" Stmt   { [$2] }
    | Body       { $1   }

-- TODO: labeled loops
Stmt :: { Stmt }
    : Selection { $1 }
    | JumpStmt  { $1 }
    | Expr      { $1 }
    | Loop      { $1 }

Selection :: { Stmt }
    : IfElse    { $1 }
    | Match     { $1 }

IfElse :: { Stmt }
    : if Term StmtBody else StmtBody    { IfElse $2 $3 $5 }
    | if Term StmtBody                  { IfElse $2 $3 [] }

-- ALLOWED CONFLICT:
--     Does: shift according to:
--         "(" Lambda ")" . Terms0
--     Could do: reduce using:
--         Term -> Lambda
-- this is desired because we want to allow
-- a lambda to be able to call
Term :: { Value }
    : literal               { $1 }
    | "[" ArrayTerms "]"    { mkArray $2 }
    | "(" TupleTerms ")"    { mkTuple $2 }
    | Lambda                { $1 }
    | CtorCall              { $1 }
    | FuncCall              { $1 }
    | "(" Term ")"          { $2 }

ArrayTerms :: { [Value] }
    : ArrayTerms "," Term   { ($3:$1) }
    | ArrayTerms ","        { $1      }
    | Term                  { [$1]    }

TupleTerms :: { [Value] }
    : TupleTerms "," Term   { ($3:$1) }
    | Term "," Term         { [$1] }

CtorCall :: { Value }
    : big_id Terms0 { CtorCall $1 $2 }

StmtBody :: { Body }
    : Stmt  { [$1] }
    | Body  { $1   }

Match :: { Stmt }
    : match Term "{" Cases1 "}" { Match $2 $4 }

Cases1 :: { [MatchCase] }
    : Cases1_   { reverse $1 }

Cases1_ :: { [MatchCase] }
    : Cases1_ Case  { ($2:$1) }
    | Case          { [$1]    }

Case :: { MatchCase }
    : Pattern BodyAssn    { ($1, $2) }

Pattern :: { Value }
    : "_"                               { $1 }
    | small_id                          { VarVal $1 }
    | "[" PatternItem "]"               { $2 }

PatternItem :: { Value }
    : literal                            { $1 }
    | TuplePattern                       { $1 }
    | CtorPattern                        { $1 }

TuplePattern :: { Value }
    : "(" TuplePtrns ")" { mkTuple (reverse $2) }

TuplePtrns :: { [Value] }
    : TuplePtrns "," Pattern   { ($3:$1)  }
    | Pattern "," Pattern      { [$3, $1] }

CtorPattern :: { Value }
    : big_id Patterns   { CtorCall $1 $2 }

Patterns :: { [Value] }
    : {- empty -}       { []      }
    | Patterns Pattern  { ($2:$1) }

JumpStmt :: { Stmt }
    : break ";"        { Break }
    | continue ";"     { Continue }
    | return Term ";"  { Return $2 }

Expr :: { Stmt }
    : NewVar        { $1         }
    | Reassignment  { $1         }
    | FuncCall ";"  { ValStmt $1 }
    | NullStmt      { $1         }

NewVar :: { Stmt }
    : let Mut small_id Type "=" Term ";"    { NewVar $2 $3 $4 $6      }
    | let Mut small_id "=" Term ";"         { NewVar $2 $3 Delayed $5 }

Mut :: { Mutability }
    : {- empty -}   { Pure   }
    | mut           { Impure }

Reassignment :: { Stmt }
    : small_id "=" Term ";" { Reassignment $1 $3 }

FuncCall :: { Value }
    : Term infix_id Term    { Application (VarVal $2) [$1, $3] }
    | Caller Terms0         { if null $2 then $1 else Application $1 $2 }

Caller :: { Value }
    : prefix_id         { VarVal $1 }
    | "(" Lambda ")"    { $2 }
    | "(" FuncCall ")"  { $2 }

-- ALLOWED CONFLICT:
--     Does: shift according to one of:
--         "(" . TupleTerms ")"
--         "(" . Term ")"
--         "(" . Lambda ")" Terms0
--         "(" . FuncCall ")" Terms0
--         "[" . ArrayTerms "]"
--         literal .
--         big_id . Terms0
--         small_id .
--         prefix_id . Terms0
--     Could do: reduce using:
--         Terms0 -> {- empty -}
Terms0 :: { [Value] }
    : Terms0_    { reverse $1 }
Terms0_ :: { [Value] }
    : {- empty -}   { []      }
    | Terms0_ Term    { ($2:$1) }

Lambda :: { Value }
    : SmallIds0 "=>" StmtBody    { Lambda $1 $3 }

NullStmt :: { Stmt }
    : ";"  { NullStmt }

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
    | Vis big_id "{" DataFields1 "}"    { Record $2 $1 $4 }

DataFields1 :: { [Field] }
    : DataFields1_  { reverse $1 }

DataFields1_ :: { [Field] }
    : DataFields1_ "," DataField    { ($3:$1) }
    | DataFields1_ ","              { $1 }
    | DataField                     { [$1] }

DataField :: { Field }
    : small_id "<" ArrowSepTypes1 ">" { Field $1 (normalize (Applied $3)) }

TypeAlias :: { Expr }
    : using Vis Type "=" Type  { TypeAlias $2 $3 $5 }

TraitDecl :: { Expr }
    : trait Vis TraitCtx big_id SmallIds1 "{" MethodDecls0 "}"    { TraitDecl $2 $3 $4 $5 $7 }

TraitCtx :: { Context }
    : {- empty -}       { [] }
    | "<" CtxSeq ">"    { $2 }

MethodDecls0 :: { [Expr] }
    : {- empty -}        { []      }
    | MethodDecls0 FuncDecl   { ($2:$1) }
    | MethodDecls0 FuncDef    { ($2:$1) }

TraitImpl :: { Expr }
    : impl TraitCtx big_id Types0 "{" MethodImpls0 "}"    { TraitImpl $2 $3 $4 $6 }

MethodImpls0 :: { [Expr] }
    : {- empty -}            { []      }
    | MethodImpls0 FuncDef    { ($2:$1) }


{
parseError :: Token -> Alex a
parseError = lexError . terse

whileLoop :: Value -> Body -> Stmt
whileLoop stmt = Loop NullStmt (ValStmt stmt) NullStmt

foreverLoop :: Body -> Stmt
foreverLoop  = Loop NullStmt NullStmt NullStmt

mkTuple :: [Value] -> Value
mkTuple vals = Tuple
    (listArray (0, length vals)
    (reverse vals))

mkArray :: [Value] -> Value
mkArray vals = Array
    (listArray (0, length vals)
    (reverse vals))
}
