{
module Front.Parser.Lexer (
    Token(..),
    Alex(..),
    runAlex,
    lexer,
    lexError,
) where

import Data.Char (digitToInt)
import Data.Int (Int64)
import Data.List (foldl')
import Text.Read (readMaybe)

import Common.SrcPos
import Common.Var
import Front.Parser.Data
import Front.Parser.Token
import Pretty
}

%wrapper "monad"

$oct_digit      = [0-7]
$sign           = [\- \+]
$big            = [A-Z]
$small          = [a-z \_]
$id_char        = [A-Z a-z 0-9 \_]
$symbol         = [\~\!\@\#\$\%\^\&\*\-\+\=\\\|\:\<\>\.\?\/]

@comment        = "--" .*
                | "{-" .* "-}"

@hexa           = (x|X) [A-Fa-f0-9]+
@decimal        = [0-9]+
@floating       = $sign? @decimal \.? (e|E) $sign? @decimal
                | $sign? @decimal \. @decimal
                | $sign? @hexa \.? (p|P) $sign? @decimal

@special        = \\ @hexa
                | \\ $oct_digit{0,3}
                | \\ [\\abfnrtv\'\"] 
@char_ch        = @special
                | [^\']
@string_ch      = @special
                | [^\"]

@big_id         = $big $id_char*
@small_id       = $small $id_char*
@operator       = $symbol+
@qual           = (@big_id \.)*


tokens :-
    <ctx_> ":"                  { reserved TColon }
    <ctx_, data_, var_> "<"     { reserved TLAngle }
    <ctx_, var_> ">"            { reserved TRAngle `andBegin` 0 }
    -- mainly for traits without a context
    <ctx_> "{"                  { reserved TLBrace `andBegin` 0 }
    <data_> ">"                 { reserved TRAngle }
    <data_> "|"                 { reserved TPipe }
    <var_> "="                  { reserved TEq `andBegin` 0 }
    <func_> @small_id           { mkVar TSmall `andBegin` ctx_ }
    <func_> @operator           { mkVar TInfix `andBegin` ctx_ }
    <0>         '\''            { begin char_ }
    <0>         '"'             { begin string_ }
    <char_> @char_ch            { char }
    <string_> @string_ch*       { string }
    <string_>   '"'             { begin 0 }
    <char_>     '\''            { begin 0 }

    $white+                     { skip }
    @comment+                   { skip }
    "="                         { reserved TEq }
    ";"                         { reserved TSemi }
    "->"                        { reserved TArrow }
    "=>"                        { reserved TEqArrow }
    ","                         { reserved TComma }
    "("                         { reserved TLParen }
    ")"                         { reserved TRParen }
    "{"                         { reserved TLBrace }
    "}"                         { reserved TRBrace }
    "["                         { reserved TLBracket }
    "]"                         { reserved TRBracket }
    "_"                         { hole }
    using                       { reserved TUsing }
    pure                        { reserved TPure `andBegin` func_ }
    impure                      { reserved TImpure `andBegin` func_ }
    let                         { reserved TLet `andBegin` var_ }
    mut                         { reserved TMut }
    intern                      { reserved TIntern }
    export                      { reserved TExport }
    import                      { reserved TImport }
    return                      { reserved TReturn }
    if                          { reserved TIf }
    else                        { reserved TElse }
    match                       { reserved TMatch }
    loop                        { reserved TLoop }
    break                       { reserved TBreak }
    continue                    { reserved TContinue }
    impl                        { reserved TImpl `andBegin` ctx_ }
    trait                       { reserved TTrait `andBegin` ctx_ }
    data                        { reserved TData `andBegin` data_ }
    $sign? 0 [Bb] [01]+         { integer 2 }
    $sign? 0 [Oo] $oct_digit+   { integer 8 }
    $sign? @decimal             { integer 10 }
    $sign? 0 @hexa              { integer 16 }
    @floating                   { float }
    @floating[Ff]               { double }
    @qual @small_id             { mkVar TSmall }
    @qual @big_id               { mkVar TBig }
    @qual @operator             { mkVar TInfix }


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

hole :: TokenAction
hole (pos, _, _, _) _ = return
    (THole (Hole (fromAlexPosn pos)))

reserved :: Token -> TokenAction
reserved tok _ 0 = lexError $ "("+|tok|+")"
reserved tok _ _ = return tok

stoi :: Int -> String -> Int64
stoi base =
    let b = fromIntegral base
        digToInt = fromIntegral . digitToInt
    in foldl' (\ !x !d ->
        b * x + digToInt d
        ) (0 :: Int64)

integer :: Int -> TokenAction
integer _ _ 0 = lexError "integral literal"
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
        Nothing -> lexError "float literal"
        Just n -> return (TValue
            (FloatLit n (fromAlexPosn pos)))

double :: TokenAction
double (pos, _, _, str) len =
    case readMaybe (take len str) :: Maybe Double of
        Nothing -> lexError "float literal"
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
char _ _ = lexError "character literal"

string :: TokenAction
string (pos, _, _, str) len = return
    (TValue (StringLit (take len str) (fromAlexPosn pos)))

lexError :: String -> Alex a
lexError msg = do
    (pos_, _, _, input) <- alexGetInput
    let pos = fromAlexPosn pos_
        lno = srcLine pos
        line = "..." ++ takeWhile (/= '\n') input
    alexError $
        "::"-|pos|-|Red|+": Error parsing a "+|msg|+":\n"
        +|Purple|+|4.>lno|+" | "+|Reset|+|line|+"\n"
        -- +|replicate (posCol + 8) ' '|+|Red|+"^"

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
