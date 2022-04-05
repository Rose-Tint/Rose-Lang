module Parser.Error (printParseErr) where

import Prelude hiding (lines)

import Data.Text (Text, unpack, lines)
import Text.Parsec (sourceLine)
import Text.Parsec.Error

import Builder.Builder
import Builder.Output


default (Int, Double)


printParseErr :: ParseError -> Text -> BuilderIO ()
printParseErr err input = 
    if errLn < length srcLines then message
        "Error while parsing %s:\n\
        \    %4s | %s\n%s\n" [
            show src,
            show (sourceLine src),
            show errLn,
            unpack (lines input !! errLn),
            errMsg
            ]
    else warn
        "$runexpected EOF resulting from:\n    %s\n"
        [errMsg]
    where
        src = errorPos err
        srcLines = lines input
        errLn = sourceLine src - 1
        errMsg = showErrorMessages "or" "unknown"
                "Expected:" "Found: " "end of input"
                (errorMessages err)


