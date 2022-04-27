{
module Parser.Parser where
}


%name parser Module
%error parseError

%tokentype Token
%token
    -- primatives
    int             { TInt $$    }
    float           { TFloat $$  }
    char            { TChar $$   }
    string          { TString $$ }

    -- identifiers
    big_id          { TBig $$    }
    small_id        { TSmall $$  }
    prefix_id       { TPrefix $$ }
    infix_id        { TInfix $$  }

    -- reserved symbols
    eq              { TEq        }
    colon           { TColon     }
    semi            { TSemi      }
    pipe            { TPipe      }
    arrow           { TArrow     }
    eq_arrow        { TEqArrow   }
    comma           { TComma     }

    -- groupers
    l_paren         { TLParen    }
    r_paren         { TRParen    }
    l_brace         { TLBrace    }
    r_brace         { TRBrace    }
    l_bracket       { TLBracket  }
    r_bracket       { TRBracket  }
    l_angle         { TLAngle    }
    r_angle         { TRAngle    }
    hole            { THole      }

    -- keywords
    pure            { TPure      }
    impure          { TImpure    }
    let             { TLet       }
    mut             { TMut       }
    intern          { TIntern    }
    extern          { TExtern    }
    module          { TModule    }
    where           { TWhere     }
    import          { TImport    }
    using           { TUsing     }
    return          { TReturn    }
    if              { TIf        }
    else            { TElse      }
    match           { TMatch     }
    loop            { TLoop      }
    break           { TBreak     }
    continue        { TContinue  }
    impl            { TImpl      }
    trait           { TTrait     }
    data            { TData      }


%%


Module :: { Module }
    : Header Imports TopLevelExprs  { Module $1 $2 $3 }

Header :: { String }
    : module big_id where           { $2 }

TopLevelExpr :: { Expr }
    : FuncDecl  { $1 }
    | FuncDef   { $1 }
    | DataDef   { $1 }
    | TypeAlias { $1 }
    | TraitDecl { $1 }
    | TraitImpl { $1 }
    -- | PragmaSeq { Left $1  }


Item :: { Item }
    : trait big_id  { TraitItem $2 }
    | data big_id   { DataItem $2  }
    | prefix_id     { FuncItem $1  }

Items1 :: { [Term] }
    : Items1_ { reverse $1 }
Items1_ :: { [Item] }
    : Item                  { [$1]  }
    | Items1_ comma         { $1    }
    | Items1_ comma Item    { $3:$1 }

Import :: { Import }
    : import big_id                                     { Import $2 Intern Nothing   }
    | import Vis big_id                                 { Import $3 $2 Nothing       }
    | import big_id using l_brace Items1 r_brace        { Import $2 Intern (Just $5) }
    | import Vis big_id using l_brace Items1 r_brace    { Import $3 $2 (Just $6)     }


Vis :: { Visibility }
    : extern        { Extern }
    | intern        { Intern }
    | {- empty -}   { Extern }

Pur :: { Purity }
    : pure      { Pure   }
    | impure    { Impure }

Mut :: { Mutability }
    : mut           { Impure   }
    | {- empty -}   { Pure   }


Type :: { Type }
    : big_id Types                  { Type $1 $2   }
    | small_id Types                { Param $1 $2  }
    | l_bracket Type r_bracket      { Type "[]" $2 }
    | l_paren CommaSepTypes r_paren { Type "," $2  }
    | l_paren ArrowSepTypes r_paren { Applied $2   }

ArrowSepTypes :: { [Type] }
    : ArrowSepTypes_    { reverse $1 }
ArrowSepTypes_ :: { [Type] }
    : ArrowSepTypes_ arrow Type { ($2:$1) }
    | Type                      { [$1]      }

CommaSepTypes :: { [Type] }
    : CommaSepTypes_    { reverse $1 }
CommaSepTypes_ :: { [Type] }
    : CommaSepTypes_ comma Type { ($2:$1) }
    | Type comma Type           { [$2, $1]  }

Types :: { [Type] }
    : Types_    { reverse $1 }
Types_ :: { [Type] }
    : Types_ Type   { ($2:$1) }
    | {- empty -}   { [] }

Constraint :: { Constraint }
    : big_id SmallIds   { Constraint $1 $2 }

SmallIds :: { [Var] }
    : SmallIds_ { reverse $1 }
SmallIds_ :: { [Var] }
    : SmallIds_ small_id    { ($2:$1) }
    | small_id              { [$1]      }

CtxSeq :: { Context }
    : CtxSeq_ colon { reverse $1 }
    | {- empty -}   { [] }
CtxSeq_ :: { Context }
    : CtxSeq_ comma Constraint  { ($2:$1) }
    | Constraint                { [$1]      }

TypeDeclNoCtx :: { TypeDecl }
    : l_angle ArrowSepTypes r_angle { TypeDecl [] (Applied $2) }

TypeDecl :: { TypeDecl }
    : l_angle CtxSeq ArrowSepTypes r_angle    { TypeDecl $2 $3 }


FuncDecl :: { Expr }
    : Pur Vis prefix_id TypeDecl    { FuncDecl $1 $2 $3 $4 }

FuncParamSeq :: { (Var, [Pattern]) }
    : InfixParamSeq     { $1 }
    | PrefixParamSeq    { reverse $1 }

InfixParamSeq :: { (Var, [Pattern]) }
    : Pattern infix_id Pattern    { ($1, [$2, $3]) }

PrefixParamSeq :: { [Pattern] }
    : PrefixParamSeq Pattern    { ($2:$1) }
    | {- empty -}       { [] }

FuncDef :: { Expr }
    : FuncParamSeq BodyAssignment   { let (name, pars) = $1 in FuncDef name pars $2 }


DataField :: { Field }
    : small_id TypeDecl { Field $1 $2 }

DataFields :: { [Field] }
    : DataFields comma DataField    { ($3:$1) }
    | DataFields comma              { $1 }
    | DataField                     { [$1] }

CtorDef :: { Ctor }
    : big_id Vis TypeDeclNoCtx                    { Ctor $1 $2 $3 }
    | big_id Vis l_bracket DataFields r_bracket   { Ctor $1 (reverse $2) $4 }

PipeSepCtors :: { [Ctor] }
    : PipeSepCtors pipe CtorDef { ($3:$1) }
    | CtorDef                   { [$1] }

DataDef :: { Expr }
    : data Vis big_id eq CtorDef PipeSepCtors { DataDef $2 $3 ($5:reverse $6) }

TypeAlias :: { Expr }
    : using Vis Type eq Type  { TypeAlias $1 $2 $4 }


TraitCtx :: { Context }
    : l_angle CtxSeq r_angle    { $2 }

TraitDecl :: { Expr }
    : trait Vis TraitCtx big_id SmallIds l_brace MethodDecls r_brace

MethodDecls :: { [Expr] }
    : Methods FuncDecl   { ($2:$1) }
    | Methods FuncDef    { ($2:$1) }
    | {- empty -}        { []      }

TraitImpl :: { Expr }
    : impl TraitCtx big_id Types l_brace MethodImpls r_brace

MethodImpls :: { [Expr] }
    : MethodImpls FuncDef    { ($2:$1) }
    | {- empty -}            { []      }


Terms0 :: { [Value] }
    : Terms0_    { reverse $1 }
Terms0_ :: { [Value] }
    : Terms0_ Term    { ($2:$1) }
    | {- empty -}   { []      }

Term :: { Value }
    : char
    | string
    | int
    | float
    | Array
    | Tuple
    | Lambda
    | CtorCall
    | FuncCall
    | l_paren Term r_paren

Array :: { Value }
    : l_bracket ArrayTerms r_bracket   { mkArray $2 }
ArrayTerms :: { [Value] }
    : ArrayTerms comma Term { ($3:$1) }
    | ArrayTerms comma      { $1      }
    | Term                  { [$1]    }

Tuple :: { Value }
    : l_paren TupleTerms r_paren    { mkTuple $2 }
TupleTerms :: { [Value] }
    : TupleTerms comma Term { ($3:$1) }
    | Term                  { [$1]    }

Lambda :: { Value }
    : SmallIds eq_arrow StmtBody    { Lambda $1 $3}

CtorCall :: { Value }
    : big_id Terms0 { CtorCall $1 $2 }

-- TODO: how to apply only to second arg in case of the
-- second option??
InfixCall :: { (Var, [Value]) }
    : Term infix_id Term    { ($2, [$1, $3]) }
    -- | infix_id Term         {}
    -- | Term infix_id         {}

FuncCall :: { Value }
    : InfixCall                         { $1                              }
    | prefix_id Term Terms0             { Application (VarVal $1) ($2:$3) }
    | prefix_id                         { VarVal $1                       }
    | l_paren Lambda r_paren Terms0     { Application $2 $4               }
    | l_paren FuncCall r_paren Terms0   { Application $2 $4               }


Pattern :: { Value }
    : hole                              { Hole                }
    | small_id                          { PtrnVal (VarVal $1) }
    | l_bracket PatternItems1 r_bracket  { PtrnMatches $1      }

-- item order doesn't matter here
PatternItems1 :: { [Value] }
    : PatternItems1 comma PatternItem    { ($3:$1) }
    | PatternItem                       { [$1]    }

PatternItem :: { Value }
    : char                               { CharVal $1   }
    | string                             { StringVal $1 }
    | int                                { IntVal $1    }
    | float                              { FloatVal $1  }
    | TuplePattern                       { $1           }
    | CtorPattern                        { $1           }

TuplePattern :: { Value }
    : l_paren TuplePtrns r_paren { mkTuple (reverse $2) }

TuplePtrns :: { [Value] }
    : TuplePtrns comma Pattern   { ($3:$1)  }
    | Pattern comma Pattern      { [$3, $1] }

CtorPattern :: { Value }
    : big_id Patterns   { CtorCall $1 $2 }

Patterns :: { [Pattern] }
    : Patterns Pattern  { ($2:$1) }
    | {- empty -}       { []      }


-- TODO: labeled loops
Stmt :: { Stmt }
    : Selection { $1 }
    | JumpStmt  { $1 }
    | ExprStmt  { $1 }
    | Loop      { $1 }
    | NullStmt  { $1 }

Body :: { Body }
    : l_brace Stmts0 r_brace    { $2 }

Stmts0 :: { [Stmt] }
    : Stmts0_    { reverse $1 }
Stmts0_ :: { [Stmt] }
    : Stmts0_ Stmt  { ($2:$1) }
    | {- empty -}   { []      }

BodyAssignment :: { Body }
    : eq Stmt   { [$2] }
    | Body      { $1   }

NullStmt :: { Stmt }
    : semi  { NullStmt }

ExprStmt :: { Stmt }
    : NewVar        { $1         }
    | Reassignment { $1         }
    | FuncCall      { ValStmt $1 }
    | NullExpr      { $1         }

JumpStmt :: { Stmt }
    : break semi        { Break }
    | continue semi     { Continue }
    | return Term semi  { Return $2 }

Selection :: { Stmt }
    : IfElse    { $1 }
    | Match     { $1 }

StmtBody :: { Body }
    : Stmt  { [$1] }
    | Body  { $1   }

IfElse :: { Stmt }
    : if Term Body                                      { IfElse $2 $3 [] }
    | if l_paren Term r_paren StmtBody                  { IfElse $3 $4 [] }
    | if Term Body else StmtBody                        { IfElse $2 $3 $5 }
    | if l_paren Term r_paren StmtBody else StmtBody    { IfElse $3 $5 $6 }

Match :: { Stmt }
    : match Term l_brace Cases1 r_brace { Match $2 $4 }

Cases1 :: { [MatchCase] }
    : Cases1_   { reverse $1 }
Cases1_ :: { [MatchCase] }
    : Cases1_ Case  { ($2:$1) }
    | Case          { [$1]    }

Case :: { MatchCase }
    : Pattern BodyAssignment    { ($1, $2) }

Loop :: { Stmt }
    : loop l_paren ExprStmt ExprStmt ExprStmt r_paren StmtBody  { Loop $3 $4 $5 $7             }
    | loop l_paren Term r_paren StmtBody                        { Loop NullStmt $3 NullStmt $5 }
    | loop Term Body                                            { Loop NullStmt $2 NullStmt $3 }

NewVar :: { Stmt }
    : let Mut small_id Type eq Term semi    { NewVar $2 $3 $4 $6      }
    | let Mut small_id eq Term semi         { NewVar $2 $3 Delayed $5 }

Reassignment :: { Stmt }
    : small_id eq Term semi { Reassignment $1 $3 }


{

mkTuple :: [Value] -> Value
mkTuple vals = Tuple (listArray (0, length vals) (reverse vals))

mkArray :: [Value] -> Value
mkArray vals = Array (listArray (0, length vals) (reverse vals))

}
