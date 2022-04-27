{
module Parser.Parser where
}


%name parser Module
%error parseError

%tokentype Token
%token
    char            { TChar $$   }
    string          { TString $$ }
    int             { TInt $$    }
    float           { TFloat $$  }
    big_id          { TBig $$    }
    small_id        { TSmall $$  }
    prefix_id       { TPrefix $$ }
    infix_id        { TInfix $$  }
    hash            { THash      }
    colon           { TColon     }
    semi            { TSemi      }
    comma           { TComma     }
    l_paren         { TLParen    }
    r_paren         { TRParen    }
    l_brace         { TLBrace    }
    r_brace         { TRBrace    }
    l_bracket       { TLBracket  }
    r_bracket       { TRBracket  }
    l_angle         { TLAngle    }
    r_angle         { TRAngle    }
    arrow           { TArrow     }
    pure            { TPure      }
    impure          { TImpure    }
    unsafe          { TUnsafe    }
    let             { TLet       }
    mut             { TMut       }
    imut            { TImut      }
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
    impl            { TImpl      }
    trait           { TTrait     }
    data            { TData      }


%%


Module :: { Module }
    : Header Imports TopLevelExprs
        { let (ps, es) = partitionEithers $3 in Module $1 $2 ps es  }

Header :: { String }
    : module big_id where                           { $2 }

TopLevelExpr :: { Either Pragma Expr }
    : FuncDecl  { Right $1 }
    | FuncDef   { Right $1 }
    | DataDef   { Right $1 }
    | TypeAlias { Right $1 }
    | TraitDecl { Right $1 }
    | TraitImpl { Right $1 }
    -- | PragmaSeq { Left $1  }


Item :: { Item }
    : trait big_id  { TraitItem $2 }
    | data big_id   { DataItem $2  }
    | prefix_id     { FuncItem $1  }

ItemList :: { [Item] }
    : ItemList comma Item
    | Item comma?

Import :: { Import }
    : import big_id                                         { Import $1 Intern Nothing }
    | import intern big_id                                  { Import $1 Intern Nothing }
    | import extern big_id                                  { Import $1 Extern Nothing }
    | import big_id using l_brace ItemList r_brace          { Import $1 Intern Nothing }
    | import intern big_id using l_brace ItemList r_brace   { Import $1 Intern Nothing }
    | import extern big_id using l_brace ItemList r_brace   { Import $1 Extern Nothing }


-- Pragma :: { Pragma }
--     : hash l_bracket Directive r_bracket    { $3 }

-- PragmaSeq :: { [Pragma] }
--     : hash l_bracket DirectiveList r_bracket    { $3 }

-- DirectiveList :: { [Pragma] }
--     : DirectiveList comma Directive { ($3 : $1) }
--     | Directive                     { ($1 : []) }

-- Directive :: { Pragma }


Visib :: { Visibility }
    : extern        { Extern }
    | intern        { Intern }
    | {- empty -}   { Extern }

Purity :: { Purity }
    : pure      { Pure   }
    | impure    { Impure }
    | unsafe    { Unsafe }

Mutab :: { Mutability }
    : mut           { Pure   }
    | imut          { Impure }
    | {- empty -}   { Pure   }


Type :: { Type }
    : big_id Types                  { Type $1 $2   }
    | small_id Types                { Param $1 $2  }
    | l_bracket Type r_bracket      { Type "[]" $2 }
    | l_paren CommaSepTypes r_paren { Type "," $2  }
    | l_paren ArrowSepTypes r_paren { Applied $2   }

ArrowSepTypes :: { [Type] }
    : ArrowSepTypes0    { reverse $1 }
ArrowSepTypes_ :: { [Type] }
    : ArrowSepTypes0 arrow Type { ($2 : $1) }
    | Type                      { [$1]      }

CommaSepTypes :: { [Type] }
    : CommaSepTypes0    { reverse $1 }
CommaSepTypes0 :: { [Type] }
    : CommaSepTypes0 comma Type { ($2 : $1) }
    | Type comma Type           { [$2, $1]  }

Types :: { [Type] }
    : Types0    { reverse $1 }
Types0 :: { [Type] }
    : Types0 Type   { ($2 : $1) }
    | {- empty -}   { [] }

Constraint :: { Constraint }
    : big_id SmallIds   { Constraint $1 $2 }

SmallIds :: { [Var] }
    : SmallIds0 { reverse $1 }
SmallIds0 :: { [Var] }
    : SmallIds0 small_id    { ($2 : $1) }
    | small_id              { [$1]      }

CtxSeq :: { Context }
    : CtxSeq0 colon { reverse $1 }
    | {- empty -}   { [] }
CtxSeq0 :: { Context }
    : CtxSeq0 comma Constraint  { ($2 : $1) }
    | Constraint                { [$1]      }

TypeDeclNoCtx :: { TypeDecl }
    : l_angle ArrowSepTypes r_angle { TypeDecl [] (Applied $2) }

TypeDecl :: { TypeDecl }
    : l_angle CtxSeq


FuncDecl :: { Expr }
    : Purity Visib prefix_id TypeDecl





(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
func-decl           = visib, purity, prefix-ident, type-decl
infix-param-seq     = pattern, infix-ident, pattern
prefix-param-seq    = prefix-ident, {pattern}
func-param-seq      = infix-param-seq | prefix-param-seq
func-def            = func-param-seq, body-assignment


(* DATATYPE DEFINITION: *)
field-decl          = small-ident, type-decl
ctor-def            = visib, big-ident, {type}
                    | visib, big-ident, [ "{", field-decl, { ",", field-decl }, [","], "}" ]
data-def            = visib, "data", big-ident, "=", ctor-def, { "|", ctor-def }
type-alias          = visib, "using", type, "=", type


(* TRAIT DEFINITION: *)
trait-ctx           = "<", constraint, { ",", constraint }, ">"
trait-decl          = visib, "trait", [trait-ctx], big-ident, small-ident, {small-ident},
                        "{", {(func-decl | func-def)}, "}"
trait-impl          = "impl", [trait-ctx], big-ident, type, {type},
                        "{", {func-decl}, "}"


(* TERMS: *)
term                = literal
                    | lambda
                    | ctor-call
                    | func-call
                    | "(", term, ")"
(* for now, lambas will be extremely limited due to
it needing `statement` or `body`, which needs `term`
(and thus `lambda'), causing a circular import. Also,
maybe it should have a unique-ish prefix for efficiency? *)
lambda              = {small-ident}, "=>", term
infix-call          = term, infix-ident, term
                    | infix-ident, term
                    | term, infix-ident
ctor-call           = big-ident, {term}
func-call           = infix-call
                    | small-ident, {term}
                    | "(", operator, ")", {term}
                    | "(", func-call, ")", {term}
                    | "(", lambda, ")", {term}
tuple               = "(", term, ",", term, { ",", term }, ")"
array               = "[", {term}, "]"


(* PATTERNS *)
ctor-pattern        = big-ident, {pattern}
tuple-pattern       = "(", pattern, ",", pattern, { ",", pattern }, ")"
pattern-item        = literal | tuple-pattern | ctor-pattern
pattern             = "_" | small-ident | "[", pattern-item, { ",", pattern-item }, "]"


(* STATEMENTS: *)
statement           = if-else | match | return | loop | new-var, ";" | reassignment, ";"
body                = "{", {statement}, "}"
body-assignment     = "=", statement | body
stmt-body           = statement | body
if-else             = "if", [heat-pragma], "(", term, ")", stmt-body,
                        [ "else", [heat-pragma], stmt-body ]
return              = "return", ( term, ";" | match | if-else )
case                = [heat-pragma], pattern, body-assignment
match               = "match","(", term, ")","{", case, {case}, "}"
loop-clause         = "(", [statement], ";", statement, ";", [statement], ")"
                    | "(", statement, ")"
loop                = "loop", loop-clause, stmt-body
var-decl-seq        = "let", mutab, small-ident, [type-decl]
assignment          = "=", term
new-var             = var-decl-seq, assignment
reassignment        = small-ident, assignment


