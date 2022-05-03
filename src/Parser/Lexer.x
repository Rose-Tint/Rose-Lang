{
module Parser.Lexer (
    Token(..),
    Alex(..),
    runAlex,
    lexer,
    alexError,
    getLexerPos,
) where

import Data.Char (digitToInt)
import Data.Int (Int64)
import Data.List (foldl')
import Text.Read (readMaybe)

import Common.SrcPos
import Common.Var
import Parser.Data
import Parser.Token
import Pretty
}

%wrapper "monad"

$oct_digit      = [0-7]
$sign           = [\- \+]
$upper          = [A-Z]
$lower          = [a-z \_]
$id_char        = [A-Z a-z 0-9 \_]
$symbol         = [\~!@#\$\%\^\&\*\-\+=\\\|:\<\>\.\?\/]

@comment        = "--" .* "\n" | "{-" .* "-}"
@hexa           = (x|X) [A-Fa-f0-9]+
@decimal        = [0-9]+
@floating       = $sign? @decimal \.? (e|E) $sign? @decimal
                | $sign? @decimal \. @decimal
                | $sign? @hexa \.? (p|P) $sign? @decimal
@character      = "\\" (@hexa
                    | $oct_digit+
                    | [\\abfnrtv\'\"])
                | .
@qualifier      = ($upper $id_char*\.)+
@big_id         = @qualifier? $upper $id_char*
@small_id       = @qualifier? $lower $id_char*
@operator       = @qualifier? $symbol+


tokens :-
    $white+                     ;
    @comment+                   ;
    "="                         { reserved TEq       }
    ":"                         { reserved TColon    }
    ";"                         { reserved TSemi     }
    "|"                         { reserved TPipe     }
    "->"                        { reserved TArrow    }
    "=>"                        { reserved TEqArrow  }
    ","                         { reserved TComma    }
    "("                         { reserved TLParen   }
    ")"                         { reserved TRParen   }
    "{"                         { reserved TLBrace   }
    "}"                         { reserved TRBrace   }
    "["                         { reserved TLBracket }
    "]"                         { reserved TRBracket }
    "<"                         { reserved TLAngle   }
    ">"                         { reserved TRAngle   }
    "_"                         { hole               }
    pure                        { reserved TPure     }
    impure                      { reserved TImpure   }
    let                         { reserved TLet      }
    mut                         { reserved TMut      }
    intern                      { reserved TIntern   }
    export                      { reserved TExtern   }
    import                      { reserved TImport   }
    using                       { reserved TUsing    }
    return                      { reserved TReturn   }
    if                          { reserved TIf       }
    else                        { reserved TElse     }
    match                       { reserved TMatch    }
    loop                        { reserved TLoop     }
    break                       { reserved TBreak    }
    continue                    { reserved TContinue }
    impl                        { reserved TImpl     }
    trait                       { reserved TTrait    }
    data                        { reserved TData     }
    $sign? @decimal             { integer 10         }
    $sign? 0 [Bb] [01]+         { integer 2          }
    $sign? 0 [Oo] $oct_digit+   { integer 8          }
    $sign? 0 @hexa              { integer 16         }
    @floating                   { float              }
    <0> '\''                    { begin char_        }
    <char_> @character          { char               }
    <char_> '\''                { begin 0            }
    <0> '"'                     { begin string_      }
    <string_> @character*       { string             }
    <string_> '"'               { begin 0            }
    "'" @character "'"          { char               }
    "\"" @character* "\""       { string             }
    <0> '`'                     { begin infix_       }
    <infix_> @big_id            { mkVar TOper        }
    <infix_> @small_id          { mkVar TOper        }
    <infix_> '`'                { begin 0            }
    @big_id                     { mkVar TBig         }
    @small_id                   { mkVar TSmall       }
    @operator                   { mkVar TOper        }


{
alexEOF :: Alex Token
alexEOF = return TEOF

type TokenAction = AlexInput -> Int -> Alex Token


fromAlexPosn :: AlexPosn -> SrcPos
fromAlexPosn (AlexPn off ln col) =
    SrcPos off ln (fromIntegral col)

mkVar :: (Var -> Token) -> TokenAction
mkVar ctor (pos, _, _, str) _ = return
    (ctor (Var (init str) (fromAlexPosn pos)))

hole :: TokenAction
hole (pos, _, _, _) _ = return
    (THole (Hole (fromAlexPosn pos)))

reserved :: Token -> TokenAction
reserved tok _inp _sc = return tok

stoi :: Int -> String -> Int64
stoi base =
    let b = fromIntegral base
        digToInt = fromIntegral . digitToInt
    in foldl' (\ !x !d ->
        b * x + digToInt d
        ) (0 :: Int64)

integer :: Int -> TokenAction
integer _ (pos, _, _, []) _ =
    lexError pos "integral literal"
integer base (pos, _, _, ('+':str)) _ = return
    (TLiteral (IntLit
        (negate (stoi base str))
        (fromAlexPosn pos)))
integer base (pos, _, _, ('-':str)) _ = return
    (TLiteral (IntLit
        (negate (stoi base str))
        (fromAlexPosn pos)))
integer base (pos, _, _, str) _ = return
    (TLiteral (IntLit
        (stoi base str)
        (fromAlexPosn pos)))

float :: TokenAction
float (pos, _, _, str) _ =
    case readMaybe str :: Maybe Double of
        Nothing -> lexError pos "float literal"
        Just n -> return (TLiteral
            (FloatLit n (fromAlexPosn pos)))

char :: TokenAction
char (pos, _, _, ('\\':ch:_)) _ =
    let ch' = case ch of
            'a' -> '\a'
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'v' -> '\v'
            _ -> ch
    in return (TLiteral
        (CharLit ch' (fromAlexPosn pos)))
char (pos, _, _, (ch:_)) _ = return
    (TLiteral (CharLit ch (fromAlexPosn pos)))
char (pos, _, _, _) _ =
    lexError pos "character literal"

string :: TokenAction
-- `init str` will throw an error if
-- `str` is empty
string (pos, _, _, "\"\"") _ = return
    (TLiteral (StringLit "" (fromAlexPosn pos)))
string (pos, _, _, str) _ = return
    (TLiteral (StringLit str (fromAlexPosn pos)))

lexError :: AlexPosn -> String -> Alex a
lexError (AlexPn _ ln col) msg = alexError $
    Red|+"\nError parsing "+|msg|+|
    Reset|+":\n    on line "+|ln|+", column "+|col

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

getLexerPos :: Alex SrcPos
getLexerPos = do
    (pos, _, _, _) <- alexGetInput
    return (fromAlexPosn pos)
}
