%wrapper "posn"

$bin_digit      = 0-1
$oct_digit      = 0-7
$dec_digit      = 0-9
$hex_digit      = [A-Fa-f0-9]
$sign           = [-+]
$upper          = A-Z
$lower          = a-z
$letter         = $upper | $lower
$id_char        = [A-Za-z0-9_]
$symbol         = [~!@#$%^&*\-+=\\|:<>.?\/]
$special        = [\\abfnrtv\'\"]
$l_paren         = "("
$r_paren         = ")"
$l_brace         = "{"
$r_brace         = "}"
$l_bracket       = "["
$r_bracket       = "]"
$l_angle         = "<"
$r_angle         = ">"

@comment        = "--" .* "\n" | "{-" .* "-}"
@binary         = ("b"|"B") $bin_digit+
@octal          = ("o"|"O") $oct_digit+
@hexa           = ("x"|"X") $hex_digit+
@decimal        = $dec_digit+
@integer        = $sign? (@decimal | "0" (@binary | @octal | @hexa))
@exponent       = "."? ("e"|"E") @sign? @decimal
@hex_exponent   = "."? ("p"|"P") @sign? @decimal
@float          = @sign? @decimal @exponent ("f"|"F")?
                | @sign? @decimal "." @decimal ("f"|"F")?
                | @sign? @hexa @hex_exponent ("f"|"F")?
@char           = "'" @character "'"
@character      = "\\" (@hexa | @octal | @special) | .
@string         = "'" @character* "'"
@literal        = @integer | @float | @char | @string
@qualifier      = @qualifier* $upper $id_char* "."
@big_id         = @qualifier? $upper $id_char*
@small_id       = @qualifier? $lower $id_char*
@operator       = @qualifier? $symbol (($letter|$symbol)+$symbol|$symbol*)
@prefix_id      = @small_id | "(" @operator ")"
@infix_id       = "`" @small_id "`" | @operator


tokens :-
    $white+         ;
    $comment+       ;
    @integer        { TInt . read   }
    @float          { TFloat . read }
    @char           { TChar         }
    @string         { TString       }
    @big_id         { TBig          }
    @small_id       { TSmall        }
    @prefix_id      { TPrefix       }
    @infix_id       { TInfix        }
    "="             { TEq           }
    ":"             { TColon        }
    ";"             { TSemi         }
    "|"             { TPipe         }
    "->"            { TArrow        }
    "=>"            { TEqArrow      }
    ","             { TComma        }
    "("             { TLParen       }
    ")"             { TRParen       }
    "{"             { TLBrace       }
    "}"             { TRBrace       }
    "["             { TLBracket     }
    "]"             { TRBracket     }
    "<"             { TLAngle       }
    ">"             { TRAngle       }
    "_"             { THole         }
    "pure"          { TPure         }
    "impure"        { TImpure       }
    "let"           { TLet          }
    "mut"           { TMut          }
    "intern"        { TIntern       }
    "extern"        { TExtern       }
    "module"        { TModule       }
    "where"         { TWhere        }
    "import"        { TImport       }
    "using"         { TUsing        }
    "return"        { TReturn       }
    "if"            { TIf           }
    "else"          { TElse         }
    "match"         { TMatch        }
    "loop"          { TLoop         }
    "break"         { TBreak        }
    "continue"      { TContinue     }
    "impl"          { TImpl         }
    "trait"         { TTrait        }
    "data"          { TData         }
