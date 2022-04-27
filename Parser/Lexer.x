{
module Parser.Lexer where

import Data.Char (digitToInt)
import Data.Int (Int64)

import Common.SrcPos
import Common.Var
import Parser.Data (Value)
}

%wrapper "posn"

$bin_digit      = 0-1
$oct_digit      = 0-7
$dec_digit      = 0-9
$hex_digit      = [A-Fa-f0-9]
$sign           = [\-\+]
$upper          = A-Z
$lower          = a-z
$letter         = [A-Za-z]
$id_char        = [A-Za-z0-9_]
$symbol         = [\~!@#\$\%\^\&\*\-\+=\\\|:\<\>\.\?\/]
$special        = [\\abfnrtv\'\"]
$l_paren        = \(
$r_paren        = \)
$l_brace        = \{
$r_brace        = \}
$l_bracket      = \[
$r_bracket      = \]
$l_angle        = \<
$r_angle        = \>

@comment        = "--" .* "\n" | "{-" .* "-}"
@binary         = ("b"|"B") $bin_digit+
@octal          = ("o"|"O") $oct_digit+
@hexa           = ("x"|"X") $hex_digit+
@decimal        = $dec_digit+
@integer        = $sign? (@decimal | "0" (@binary | @octal | @hexa))
@exponent       = "."? ("e"|"E") $sign? @decimal
@hex_exponent   = "."? ("p"|"P") $sign? @decimal
@float          = $sign? @decimal @exponent ("f"|"F")?
                | $sign? @decimal "." @decimal ("f"|"F")?
                | $sign? @hexa @hex_exponent ("f"|"F")?
@character      = "\\" (@hexa | @octal | $special) | .
@char           = "'" @character "'"
@string         = "'" @character* "'"
@literal        = @integer | @float | @char | @string
@qualifier      = ($upper $id_char* ".")+
@big_id         = @qualifier? $upper $id_char*
@small_id       = @qualifier? $lower $id_char*
@operator       = @qualifier? $symbol (($letter|$symbol)+$symbol|$symbol*)
@prefix_id      = @small_id | "(" @operator ")"
@infix_id       = "`" @small_id "`" | @operator


tokens :-
    $white+         ;
    @comment+       ;
    @integer        { integer                  }
    @float          { float                    }
    @char           { char                     }
    @string         { string                   }
    @big_id         { mkVar TBig               }
    @small_id       { mkVar TSmall             }
    @prefix_id      { mkVar TPrefix            }
    @infix_id       { mkVar TInfix             }
    "="             { reserved TEq             }
    ":"             { reserved TColon          }
    ";"             { reserved TSemi           }
    "|"             { reserved TPipe           }
    "->"            { reserved TArrow          }
    "=>"            { reserved TEqArrow        }
    ","             { reserved TComma          }
    "("             { reserved TLParen         }
    ")"             { reserved TRParen         }
    "{"             { reserved TLBrace         }
    "}"             { reserved TRBrace         }
    "["             { reserved TLBracket       }
    "]"             { reserved TRBracket       }
    "<"             { reserved TLAngle         }
    ">"             { reserved TRAngle         }
    "_"             { hole                     }
    "pure"          { reserved TPure           }
    "impure"        { reserved TImpure         }
    "let"           { reserved TLet            }
    "mut"           { reserved TMut            }
    "intern"        { reserved TIntern         }
    "extern"        { reserved TExtern         }
    "module"        { reserved TModule         }
    "where"         { reserved TWhere          }
    "import"        { reserved TImport         }
    "using"         { reserved TUsing          }
    "return"        { reserved TReturn         }
    "if"            { reserved TIf             }
    "else"          { reserved TElse           }
    "match"         { reserved TMatch          }
    "loop"          { reserved TLoop           }
    "break"         { reserved TBreak          }
    "continue"      { reserved TContinue       }
    "impl"          { reserved TImpl           }
    "trait"         { reserved TTrait          }
    "data"          { reserved TData           }


{
type TokenAction = AlexPosn -> String -> Token


mkVar :: (Var -> Token) -> TokenAction
mkVar ctor (AlexPn off ln col) str =
    ctor (Var str (SrcPos off ln col))

hole :: TokenAction
hole (AlexPn off ln col) _ =
    THole (Hole (SrcPos off ln col))

reserved :: Token -> TokenAction
reserved t _ _ = t

integer :: TokenAction
integer (AlexPn _ ln _) [] = error
    ("error lexing integer on line " ++ show ln)
integer (AlexPn off ln col) str =
    let !n = foldl' (\ !x d ->
        base * x + fromIntegral (digitToInt d)
        ) (0 :: Int64) str
    in TInt (IntVal n (SrcPos off ln col))

float :: TokenAction
float (AlexPn off ln col) str =
    let !n = read str :: Double
    in TFloat (FloatVal n (SrcPos off ln col))

char :: TokenAction
char (AlexPn _ ln _) [] = error
    ("error lexing character literal on line " ++ show ln)
char (AlexPn off ln col) (_:'\\':ch:rest) =
    let ch' = case ch of
            'a' -> '\a'
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'v' -> '\v'
            _ -> ch
    in TChar (CharVal ch' (SrcPos off ln col))
char (AlexPn off ln col) str =
    let !ch = head (tail str)
    in TChar (CharVal ch (SrcPos off ln col))

string :: TokenAction
string (AlexPn off ln col) [_, _] =
    TString (StringVal "" (SrcPos off ln col))
string (AlexPn off ln col) (_:str@(_:_)) =
    let !s = init str
    in TString (StringVal s (SrcPos off ln col))
string (AlexPn _ ln _) _ = error
    ("error lexing string literal on line " ++ show ln)


data Token
    = TInt Int64
    | TFloat Double
    | TChar Char
    | TString String
    | TBig String
    | TSmall String
    | TPrefix String
    | TInfix String
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
    | THole
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
}
