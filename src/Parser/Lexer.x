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
$big            = [A-Z]
$small          = [a-z \_]
$id_char        = [A-Z a-z 0-9 \_]
$symbol         = [\~\!\@\#\$\%\^\&\*\-\+\=\\\|\:\<\>\.\?\/]

@comment        = "--" .* "\n" | "{-" .* "-}"

@hexa           = (x|X) [A-Fa-f0-9]+
@decimal        = [0-9]+
@floating       = $sign? @decimal \.? (e|E) $sign? @decimal
                | $sign? @decimal \. @decimal
                | $sign? @hexa \.? (p|P) $sign? @decimal

@character      = \\ @hexa
                | \\ $oct_digit+
                | \\ [\\abfnrtv\'\"]
                | ~\"

@big_id         = $big $id_char*
@small_id       = $small $id_char*
@operator       = $symbol+
@qual           = (@big_id \.)*


tokens :-
    $white+                     { skip               }
    @comment+                   { skip               }
    \=                          { reserved TEq       }
    \:                          { reserved TColon    }
    \;                          { reserved TSemi     }
    \|                          { reserved TPipe     }
    \-\>                        { reserved TArrow    }
    \=\>                        { reserved TEqArrow  }
    \,                          { reserved TComma    }
    \(                          { reserved TLParen   }
    \)                          { reserved TRParen   }
    \{                          { reserved TLBrace   }
    \}                          { reserved TRBrace   }
    \[                          { reserved TLBracket }
    \]                          { reserved TRBracket }
    \<                          { reserved TLAngle   }
    \>                          { reserved TRAngle   }
    \_                          { hole               }
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
    $sign? 0 [Bb] [01]+         { integer 2          }
    $sign? 0 [Oo] $oct_digit+   { integer 8          }
    $sign? @decimal             { integer 10         }
    $sign? 0 @hexa              { integer 16         }
    @floating                   { float              }
    @floating[Ff]               { double             }
    <char_> @character          { char               }
    <string_> @character*       { string             }
    <infix_> @qual @small_id    { mkVar TInfix       }
    \( @qual @operator \)       { prefixOper         }
    @qual @big_id               { mkVar TBig         }
    @qual @small_id             { mkVar TSmall       }
    @qual @operator             { mkVar TInfix       }

    <0>         '\''            { begin char_        }
    <0>         '"'             { begin string_      }
    <0>         '`'             { begin infix_       }
    <string_>   '"'             { begin 0            }
    <char_>     '\''            { begin 0            }
    <infix_>      '`'           { begin 0            }


{
alexEOF :: Alex Token
alexEOF = return TEOF

type TokenAction = AlexInput -> Int -> Alex Token


fromAlexPosn :: AlexPosn -> SrcPos
fromAlexPosn (AlexPn off ln col) =
    SrcPos off ln (fromIntegral col)

mkVar :: (Var -> Token) -> TokenAction
mkVar ctor (pos, _, _, str) len = return
    (ctor (Var (take len str) (fromAlexPosn pos)))

prefixOper :: TokenAction
prefixOper (pos, _, _, str) len = return
    (TPrefix (Var (take len str) (fromAlexPosn pos)))

hole :: TokenAction
hole (pos, _, _, _) _ = return
    (THole (Hole (fromAlexPosn pos)))

reserved :: Token -> TokenAction
reserved tok _ _ = return tok

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
integer base (pos, _, _, ('+':str)) len = return
    (TValue (IntLit
        (negate (stoi base (take len str)))
        (fromAlexPosn pos)))
integer base (pos, _, _, ('-':str)) len = return
    (TValue (IntLit
        (negate (stoi base (take len str)))
        (fromAlexPosn pos)))
integer base (pos, _, _, str) len = return
    (TValue (IntLit
        (stoi base (take len str))
        (fromAlexPosn pos)))

float :: TokenAction
float (pos, _, _, str) len =
    case readMaybe (take len str) :: Maybe Float of
        Nothing -> lexError pos "float literal"
        Just n -> return (TValue
            (FloatLit n (fromAlexPosn pos)))

double :: TokenAction
double (pos, _, _, str) len =
    case readMaybe (take len str) :: Maybe Double of
        Nothing -> lexError pos "float literal"
        Just n -> return (TValue
            (DoubleLit n (fromAlexPosn pos)))

char :: TokenAction
char (pos, _, _, ('\\':ch:_)) _len =
    let ch' = case ch of
            'a' -> '\a'
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'v' -> '\v'
            _ -> ch
    in return (TValue
        (CharLit ch' (fromAlexPosn pos)))
char (pos, _, _, (ch:_)) 1 = return
    (TValue (CharLit ch (fromAlexPosn pos)))
char (pos, _, _, _) _ =
    lexError pos "character literal"

string :: TokenAction
string (pos, _, _, str) len = return
    (TValue (StringLit (take len str) (fromAlexPosn pos)))

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
