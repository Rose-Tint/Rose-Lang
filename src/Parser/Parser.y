{
module Parser.Parser (runAlex, rose, replP) where

import Data.Array (listArray)

import Common.Module
import Common.Specifiers
import Common.Var
import AST
import Parser.Lexer
import Text.Pretty
import Typing.Constraint
import Typing.Type
import Typing.TypeDecl
}

{- TODO:
 - FFI
 - seperate imports from top-level expressions
 -}

{- R/R CONFLICT:
	Type -> small_id .                                  (rule 29)
	SmallIds1_ -> small_id .                            (rule 110)

	small_id       reduce using rule 29
			(reduce using rule 110)
-}

%name rose Module
%name replP Term
%error { parseError }
%lexer { lexer } { TEOF }
%monad { Alex }

%tokentype { Token }
%token
    -- keywords
    pure            { TPure     }
    impure          { TImpure   }
    let             { TLet      }
    in              { TIn      }
    extern          { TExtern   }
    import          { TImport   }
    return          { TReturn   }
    if              { TIf       }
    then            { TThen     }
    else            { TElse     }
    match           { TMatch    }
    loop            { TLoop     }
    break           { TBreak    }
    continue        { TContinue }
    impl            { TImpl     }
    trait           { TTrait    }
    data            { TData     }
    -- reserved symbols
    "="             { TEq       }
    ":"             { TColon    }
    ";"             { TSemi     }
    "|"             { TPipe     }
    "->"            { TArrow    }
    "=>"            { TEqArrow  }
    ","             { TComma    }
    -- groupers
    "("             { TLParen   }
    ")"             { TRParen   }
    "{"             { TLBrace   }
    "}"             { TRBrace   }
    "["             { TLBracket }
    "]"             { TRBracket }
    "<"             { TLAngle   }
    ">"             { TRAngle   }
    -- literals
    literal         { TValue $$ }
    -- identifiers
    big_id          { TBig $$   }
    small_id        { TSmall $$ }
    infix_id        { TInfix $$ }
    "_"             { THole $$  }

%nonassoc infix_id
%left big_id small_id
%nonassoc literal

-- ghost precedence for function application
-- https://stackoverflow.com/questions/27630269
%nonassoc APP

%right if then else "=>" "(" "[" ";"
%nonassoc return


%%

Module :: { Module }
    : Imports0 TopLevelExprs0  { Module (reverse $1) (reverse $2) }

Imports0 :: { [Import] }
    : {- empty -}       { [] }
    | Imports0 Import   { ($2:$1) }

Import :: { Import }
    : import big_id { Import $2 }

TopLevelExprs0 :: { [Expr] }
    : {- empty -}                   { [] }
    | TopLevelExprs0 TopLevelExpr    { ($2:$1) }

TopLevelExpr :: { Expr }
    : FuncDecl   { $1 }
    | FuncDef    { $1 }
    | DataDef    { $1 }
    | TraitDecl  { $1 }
    | TraitImpl  { $1 }

FuncDecl :: { Expr }
    : Pur small_id TypeDecl            { FuncDecl $1 $2 $3 }
    | Pur "(" infix_id ")" TypeDecl    { FuncDecl $1 $3 $5 }
    | extern Pur small_id TypeDecl     { FuncDecl $2 $3 $4 }

Pur :: { Purity }
    : pure      { Pure }
    | impure    { Impure }

TypeDecl :: { Type }
    : "<" CtxSeq ":" ArrowSepTypes1 ">" { $4 }
    | "<" ArrowSepTypes1 ">"            { $2 }

CtxSeq :: { [Constraint] }
    : CtxSeq "," Constraint { ($3:$1) }
    | Constraint            { [$1]    }

Constraint :: { Constraint }
    : big_id SmallIds0   { Constraint $1 $2 }

-- ArrowSepTypes1 :: { Type }
--     : ArrowSepTypes1_    { $1 }

ArrowSepTypes1 :: { Type }
    : Type                      { $1 }
    | ArrowSepTypes1 "->" Type { $1 :-> $3 }

Type :: { Type }
    : big_id Types0             { Type $1 $2 }
    | small_id                  { TypeVar $1 }
    | "[" Type "]"              { ArrayType $2 }
    | "(" CommaSepTypes2 ")"    { TupleType $2 }
    | "(" ArrowSepTypes1 ")"    { $2 }

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
    : Type           { ([$1]) }
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

Params0 :: { [Pattern] }
    : Params0_  { reverse $1 }

Params0_ :: { [Pattern] }
    : {- empty -}                { [] }
    | Params0_ Pattern  { ($2:$1) }

BodyAssn :: { Stmt }
    : "=" TermStmt   { Return $2 }
    | Body       { $1   }

-- TODO: labeled loops
Stmt :: { Stmt }
    : IfElse            { $1 }
    | Match             { $1 }
    | break ";"         { Break }
    | continue ";"      { Continue }
    | return TermStmt   { Return $2 }
    | Expr              { $1 }
    | Loop              { $1 }

IfElse :: { Stmt }
    : if Term then StmtBody else StmtBody    { IfElse $2 $4 $6 }
    | if Term then StmtBody                  { IfElse $2 $4 NullStmt }

Term :: { Value }
    : literal               { Literal $1 }
    | "[" ArrayTerms1 "]"   { mkArray $2 }
    | "(" TupleTerms2 ")"   { mkTuple $2 }
    | SmallIds1             {
        let (var:vars) = fmap VarVal $1
        in valueFromList var vars
        }
    | LetIn                 { $1 }
    | Lambda                { $1 }
    | FuncCall              { $1 }
    | "(" Term ")"          { $2 }

ArrayTerms1 :: { [Value] }
    : ArrayTerms1 "," Term   { ($3:$1) }
    | ArrayTerms1 ","        { $1      }
    | Term                   { [$1]    }

TupleTerms2 :: { [Value] }
    : TupleTerms2 "," Term   { ($3:$1) }
    | Term "," Term         { [$1] }

StmtBody :: { Stmt }
    : Stmt  { $1 }
    | Body  { $1 }

Match :: { Stmt }
    : MatchHead "{" StmtCases1 "}" { Match $1 $3 }

MatchHead :: { Value }
    : match Term { $2 }

StmtCases1 :: { [StmtCase] }
    : StmtCases1 StmtCase   { ($2:$1) }
    | StmtCase          { [$1] }

Pattern :: { Pattern }
    : "_"                   { $1 }
    | small_id              { Param $1 }
    | "[" PatternItem "]"   { $2 }

PatternItem :: { Pattern }
    : literal       { LitPtrn $1 }
    | TuplePattern  { $1 }
    | CtorPattern   { $1 }

TuplePattern :: { Pattern }
    : "(" CommaSepPtrn2 ")" { TuplePtrn $2 }

CommaSepPtrn2 :: { [Pattern] }
    : CommaSepPtrn2_ { reverse $1 }

CommaSepPtrn2_ :: { [Pattern] }
    : CommaSepPtrn2_ "," Pattern   { ($3:$1)  }
    | Pattern "," Pattern          { [$3, $1] }

CtorPattern :: { Pattern }
    : big_id Patterns0   { CtorPtrn $1 $2 }

Patterns0 :: { [Pattern] }
    : Patterns0_ { reverse $1 }

Patterns0_ :: { [Pattern] }
    : {- empty -}       { [] }
    | Patterns0 Pattern  { ($2:$1) }

StmtCase :: { StmtCase }
    : Pattern "->" Expr { StmtCase $1 (
        case $3 of
            ValStmt val -> Return val
            stmt -> stmt)}
    | Pattern Body      { StmtCase $1 $2 }

Expr :: { Stmt }
    : NewVar                { $1 }
    | small_id "=" TermStmt { Reassignment $1 $3 }
    | TermStmt              { ValStmt $1 }
    | ";"                   { NullStmt }

NewVar :: { Stmt }
    : let small_id NewVarTypeDecl "=" TermStmt  { NewVar $2 $3 $5 }

NewVarTypeDecl :: { Maybe Type }
    : TypeDecl      { Just $1 }
    | {- empty -}   { Nothing }

FuncCall :: { Value }
    : Term infix_id         { Application (VarVal $2) $1 }
    | "(" infix_id ")"      { VarVal $2 }
    | big_id                { CtorCall $1 }
    | Term Term %prec APP   { Application $1 $2 }

TermStmt :: { Value }
    : Term ";" { $1 }
    -- | if Term then Term else Term { IfElseVal $2 $4 $6 }
    | MatchHead "{" TermCases "}" { MatchVal $1 $3 }

TermCases :: { [ValCase] }
    : Pattern "=>" TermStmt { [ValCase $1 $3] }
    | TermCases Pattern "=>" TermStmt { (ValCase $2 $4:$1) }

Lambda :: { Value }
    : SmallIds1 "=>" Term    { mkLambda $1 $3 }

SmallIds1 :: { [Var] }
    : SmallIds1_    { reverse $1 }

-- the %prec here somehow resolves a r/r conflict :idk:
SmallIds1_ :: { [Var] }
    : SmallIds1_ small_id   { ($2:$1) }
    | small_id %prec APP    { [$1] }

SmallIds0 :: { [Var] }
    : SmallIds1_    { reverse $1 }
    | {- empty -}   { [] }

LetIn :: { Value }
    : let small_id "=" Term in Term { LetIn $2 $4 $6 }

Loop :: { Stmt }
    : loop "(" Expr Expr Expr ")" StmtBody  { Loop $3 $4 $5 $7 }
    | loop Term StmtBody                    { whileLoop $2 $3 }
    | loop Body                             { foreverLoop $2 }

Body :: { Stmt }
    : "{" Stmts0 "}"    { Compound $2 }

Stmts0 :: { [Stmt] }
    : Stmts0_    { reverse $1 }

Stmts0_ :: { [Stmt] }
    : {- empty -}   { []      }
    | Stmts0_ Stmt  { ($2:$1) }

DataDef :: { Expr }
    : data big_id SmallIds0 CtorList    { DataDef $2 $3 $4 }

CtorList :: { [Ctor] }
    : "=" CtorDef           { [$2] }
    | CtorList "|" CtorDef  { ($3:$1) }

CtorDef :: { Ctor }
    : big_id                        { SumType $1 [] }
    | big_id "<" ArrowSepTypes1 ">" { SumType $1 (typeToList $3) }

TraitDecl :: { Expr }
    : trait TraitCtx big_id SmallIds1 "{" MethodDecls1 "}"    { TraitDecl $2 $3 $4 $6 }

TraitCtx :: { Context }
    : {- empty -}       { Ctx [] }
    | "<" CtxSeq ">"    { Ctx $2 }

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

whileLoop :: Value -> Stmt -> Stmt
whileLoop val = Loop NullStmt (ValStmt val) NullStmt

foreverLoop :: Stmt -> Stmt
foreverLoop = Loop NullStmt NullStmt NullStmt

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
