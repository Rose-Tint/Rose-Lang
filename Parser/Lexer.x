{
module Parser.Lexer (
    Token(..),
    alexScanTokens
) where

import Data.Char (digitToInt)
import Data.Int (Int64)
import Data.List (foldl')

import Common.SrcPos
import Common.Var
import Parser.Data
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


fromAlexPosn :: AlexPosn -> SrcPos
fromAlexPosn (AlexPn off ln col) = SrcPos off ln (fromIntegral col)

mkVar :: (Var -> Token) -> TokenAction
mkVar ctor pos str =
    ctor (Var str (fromAlexPosn pos))

hole :: TokenAction
hole pos _ =
    THole (Hole (fromAlexPosn pos))

reserved :: Token -> TokenAction
reserved t _ _ = t

integer :: TokenAction
integer (AlexPn _ ln _) [] = error
    ("error lexing integer on line " ++ show ln)
integer pos str =
    let !n = foldl' (\ !x d ->
            (10 :: Int64) * x + fromIntegral (digitToInt d)
            ) (0 :: Int64) str
    in TInt (IntLit n (fromAlexPosn pos))

float :: TokenAction
float pos str =
    let !n = read str :: Double
    in TFloat (FloatLit n (fromAlexPosn pos))

char :: TokenAction
char (AlexPn _ ln _) [] = error
    ("error lexing character literal on line " ++ show ln)
char pos (_:'\\':ch:_) =
    let ch' = case ch of
            'a' -> '\a'
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'v' -> '\v'
            _ -> ch
    in TChar (CharLit ch' (fromAlexPosn pos))
char pos str =
    let !ch = head (tail str)
    in TChar (CharLit ch (fromAlexPosn pos))

string :: TokenAction
string pos [_, _] =
    TString (StringLit "" (fromAlexPosn pos))
string pos (_:str@(_:_)) =
    let !s = init str
    in TString (StringLit s (fromAlexPosn pos))
string (AlexPn _ ln _) _ = error
    ("error lexing string literal on line " ++ show ln)


data Token
    = TInt Value
    | TFloat Value
    | TChar Value
    | TString Value
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
}
