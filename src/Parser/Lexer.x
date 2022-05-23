{
module Parser.Lexer (
    Token(..),
    Alex(..),
    runAlex,
    lexer,
    lexError,
) where

import Text.Read (readMaybe)

import Common.SrcPos
import Common.Var
import AST
import Parser.Token
import Text.Pretty
import Utils.String
}

%wrapper "monad"

$oct_digit      = [0-7]
$hex_digit      = [A-Fa-f0-9]
$sign           = [\- \+]
$big            = [A-Z]
$small          = [a-z \_]
$id_char        = [A-Z a-z 0-9 \_]
$symbol         = [\~\!\@\#\$\%\^\&\*\-\+\=\\\|\:\<\>\.\?\/]

@comment        = "--" .*
                | "{-" .* "-}"

@hex            = [Xx] $hex_digit+
@octal          = [Oo] $oct_digit+
@decimal        = [0-9]+
@floating       = $sign? @decimal \.? [Ee] $sign? @decimal
                | $sign? @decimal \. @decimal
                | $sign? @hex \.? [Pp] $sign? @decimal

@special        = \\ @decimal
                | \\ [Oo] $oct_digit{0,3}
                | \\ [Xx] $hex_digit{0,2}
                | \\ [Uu] $hex_digit{4}
                | \\ [abfnrtv\\\'\"]
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

    <0> \'                      { begin char_ }
    <char_> @char_ch            { char }
    <char_> \'                  { begin 0 }
    <0> \"                      { begin string_ }
    <string_> @string_ch*       { string }
    <string_> \"                { begin 0 }

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
    extern                      { reserved TExtern }
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
    $sign? 0 @hex              { integer 16 }
    @floating                   { float }
    @floating[Ff]               { double }
    @qual @small_id             { mkVar TSmall }
    @qual @big_id               { mkVar TBig }
    @qual @operator             { mkVar TInfix }


{
alexEOF :: Alex Token
alexEOF = return TEOF

type TokenAction = AlexInput -> Int -> Alex Token


fromAlexPosn :: AlexPosn -> Int -> SrcPos
fromAlexPosn (AlexPn off ln col) _len =
    let col' = fromIntegral col
    in SrcPos off ln col'

mkVar :: (Var -> Token) -> TokenAction
mkVar ctor (pos, _, _, str) len = return
    (ctor (Var (take len str) (fromAlexPosn pos len)))

hole :: TokenAction
hole (pos, _, _, _) _ = return
    (THole (Hole (fromAlexPosn pos 1)))

reserved :: (SrcPos -> Token) -> TokenAction
reserved tok (pos, _, _, _) len = return
    (tok (fromAlexPosn pos len))

integer :: Int -> TokenAction
integer _ _ 0 = lexError "integral literal"
integer base (pos, _, _, ('+':str)) len = return
    (TValue (IntLit
        (negate (readInt base (take len str)))
        (fromAlexPosn pos len)))
integer base (pos, _, _, ('-':str)) len = return
    (TValue (IntLit
        (negate (readInt base (take len str)))
        (fromAlexPosn pos len)))
integer base (pos, _, _, str) len = return
    (TValue (IntLit
        (readInt base (take len str))
        (fromAlexPosn pos len)))

float :: TokenAction
float (pos, _, _, str) len =
    case readMaybe (take len str) :: Maybe Float of
        Nothing -> lexError "float literal"
        Just n -> return (TValue
            (FloatLit n (fromAlexPosn pos len)))

double :: TokenAction
double (pos, _, _, str) len =
    case readMaybe (take len str) :: Maybe Double of
        Nothing -> lexError "float literal"
        Just n -> return (TValue
            (DoubleLit n (fromAlexPosn pos len)))

char :: TokenAction
char (pos, _, _, ('\\':ch:_)) len =
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
        (CharLit ch' (fromAlexPosn pos len)))
char (pos, _, _, (ch:_)) 1 = return
    (TValue (CharLit ch (fromAlexPosn pos 1)))
char _ _ = lexError "character literal"

string :: TokenAction
string (pos, _, _, str) len = return
    (TValue (StringLit (take len str) (fromAlexPosn pos len)))

lexError :: String -> Alex a
lexError msg = do
    (pos_, _, _, input) <- alexGetInput
    let pos = fromAlexPosn pos_ 0
        lno = posLine pos
        line = "..." ++ takeWhile (/= '\n') input
    alexError $
        "::"-|pos|-": $rError parsing a "+|msg|+
        ":\n$p"+|4.>lno|+" | $R"+|line|+"\n"

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
