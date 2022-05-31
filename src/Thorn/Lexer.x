{
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Thorn.Lexer (
    Alex(..),
    runAlex,
    lexer,
    lexError,
) where

import qualified Data.ByteString.Lazy.Char8 as BS

import Common.Module
import Common.SrcPos
import Thorn.Token
import Text.Pretty
}

%wrapper "monad-bytestring"

$id_char    = [A-Z a-z 0-9 \_ \- \. \/ \\]
$non_newl   = [\ \t \f \v \r]
$dir_sep    = [\/ \\ \: \>]
$path_char  = [a-z A-Z 0-9 \- \/ \\ \: \>]

$name_start = [a-z A-Z]
$name_char  = [a-z A-Z 0-9]

@comment    = "--" .*
            | "{-" .* "-}"

@home_dir   = "~" | "&" | "@" | "SYS$LOGIN:"
@parent_dir = "."* $dir_sep+
@path_start = @home_dir $dir_sep
            | @parent_dir+
@path       = @path_start $path_char+
@name_tail  = ("-" $name_char+)+
            | $name_char+
@name       = $name_start @name_tail

@mod_seg    = [A-Z][a-z A-Z 0-9]+
@module     = @mod_seg ("." @mod_seg)*


tokens :-
    @comment+           { skip }
    $white              { skip }

    "project"           { reserved KW_project }
    "build-dir"         { reserved KW_build_dir }
    "source-dir"        { reserved KW_source_dir }
    "modules"           { reserved KW_modules }
    "executable"        { reserved KW_executable }
    "library"           { reserved KW_library }
    ","                 { reserved TComma }
    ":"                 { reserved TColon }
    ";"                 { reserved TSemi }
    "{"                 { reserved TLBrace }
    "}"                 { reserved TRBrace }

    @name               { mkName }
    @module             { mkModule }

    "`"                 { begin sc_path }
    <sc_path> @path     { mkPath }
    <sc_path> "`"       { begin 0 }

-- this lets the error messaging give a the proper position
{alexEOF :: Alex Token
alexEOF = return TEOF


type TokenAction = AlexInput -> Int64 -> Alex Token


fromAlexPosn :: AlexPosn -> Int64 -> SrcPos
fromAlexPosn (AlexPn off ln col) _len =
    let col' = fromIntegral col
    in SrcPos off ln col'

mkPath :: TokenAction
mkPath (_pos, _, bs, _) len = return
    (TPath (BS.unpack (BS.take len bs)))

mkName :: TokenAction
mkName (_pos, _, bs, _) len = return
    (TName (BS.unpack (BS.take len bs)))

mkModule :: TokenAction
mkModule (_pos, _, bs, _) len = return (TModule
    (strToMod (BS.unpack (BS.take len bs))))

reserved :: Token -> TokenAction
reserved tok _ _len = return tok

lexError :: String -> Alex a
lexError msg = do
    (aPos, _, input, _) <- alexGetInput
    let pos = fromAlexPosn aPos 0
        lno = posLine pos
        line = BS.append
            (BS.pack "...")
            (BS.takeWhile (/= '\n') input)
    alexError $
        "::"-|pos|-": $rError parsing the token "+|msg|+
        ":\n$p"+|4.>posLine pos
        |+" | $R"+|BS.unpack line|+"\n"

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
