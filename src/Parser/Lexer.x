{
module Parser.Lexer (
    Token(..),
    Alex(..),
    runAlex,
    lexer,
    lexError,
) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Read (readMaybe)

import Common.SrcPos
import Common.Var
import AST
import Parser.Token
import Text.Pretty
import Utils.String
}

%wrapper "monad-bytestring"

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
    pure                        { reserved TPure `andBegin` func_ }
    impure                      { reserved TImpure `andBegin` func_ }
    let                         { reserved TLet `andBegin` var_ }
    in                          { reserved TIn }
    extern                      { reserved TExtern `andBegin` func_ }
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
    $sign? 0 @hex               { integer 16 }
    @floating                   { float }
    @floating[Ff]               { double }
    @qual @small_id             { mkVar TSmall }
    @qual @big_id               { mkVar TBig }
    @qual @operator             { mkVar TInfix }

-- this lets the error messaging give a the proper position
{alexEOF :: Alex Token
alexEOF = return TEOF


type TokenAction = AlexInput -> Int64 -> Alex Token


fromAlexPosn :: AlexPosn -> Int64 -> SrcPos
fromAlexPosn (AlexPn off ln col) _len =
    let col' = fromIntegral col
    in SrcPos off ln col'

mkVar :: (Var -> Token) -> TokenAction
mkVar ctor (pos, _, str, _) len = return (ctor (Var
    (BS.unpack$ BS.take len str)
    (fromAlexPosn pos len)))

hole :: TokenAction
hole (pos, _, _, _) _ = return
    (THole (Hole (fromAlexPosn pos 1)))

reserved :: Token -> TokenAction
reserved tok _ _len = return tok

integer :: Int -> TokenAction
integer _ _ 0 = lexError "integral literal"
{-
integer base (pos, _, _, ('+':str)) len = return
    (TValue (IntLit
        (negate (readInt base (BS.unpack $ BS.take len str)))
        (fromAlexPosn pos len)))
integer base (pos, _, _, ('-':str)) len = return
    (TValue (IntLit
        (negate (readInt base (BS.unpack $ BS.take len str)))
        (fromAlexPosn pos len)))
-}
integer base (pos, _, str, _) len = return
    (TValue (IntLit
        (readInt base (BS.unpack $ BS.take len str))
        (fromAlexPosn pos len)))

float :: TokenAction
float (pos, _, bs, _) len =
    let str = BS.unpack (BS.take len bs)
    in case readMaybe str :: Maybe Float of
        Nothing -> lexError "float literal"
        Just n -> return (TValue
            (FloatLit n (fromAlexPosn pos len)))

double :: TokenAction
double (pos, _, bs, _) len =
    let str = BS.unpack (BS.take len bs)
    in case readMaybe str :: Maybe Double of
        Nothing -> lexError "float literal"
        Just n -> return (TValue
            (DoubleLit n (fromAlexPosn pos len)))

char :: TokenAction
char (pos, _, bs, _) len = case BS.uncons bs of
    Just (ch, bs') ->
        let ch' = case ch of
                '\\' -> case BS.head bs' of
                    'a' -> '\a'
                    'b' -> '\b'
                    'f' -> '\f'
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    'v' -> '\v'
                    _ -> ch
                _ -> ch
        in return (TValue
            (CharLit ch (fromAlexPosn pos len)))
    Nothing -> lexError "character literal"

string :: TokenAction
string (pos, _, str, _) len = return
    (TValue (StringLit
        (BS.unpack (BS.take len str))
        (fromAlexPosn pos len)))

lexError :: String -> Alex a
lexError msg = do
    (aPos, _, input, _) <- alexGetInput
    let pos = fromAlexPosn aPos 0
        lno = posLine pos
        line = BS.append
            (BS.pack "...")
            (BS.takeWhile (/= '\n') input)
    alexError $
        "::"-|pos|-": $rError parsing a "+|msg|+
        ":\n$p"+|4.>posLine pos
        |+" | $R"+|BS.unpack line|+"\n"

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
