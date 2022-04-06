module Parser.Error (prettyParseErr) where

import Prelude hiding (lines)

import Data.Text (Text, unpack, lines)
import Text.Parsec (sourceLine)
import Text.Parsec.Error

import Color


default (Int, Double)


prettyParseErr :: ParseError -> Text -> String
prettyParseErr err input = 
    if errLn < length srcLines then printf
        "Error while parsing %s:\n\
        \    %4d | %s\n%s\n"
            (show src)
            (sourceLine src)
            (show errLn)
            (unpack (lines input !! errLn))
            errMsg
    else printf
        "$runexpected EOF resulting from:\n    %s\n"
        errMsg
    where
        src = errorPos err
        srcLines = lines input
        errLn = sourceLine src - 1
        errMsg = showErrorMessages "or" "unknown"
                "Expected:" "Found: " "end of input"
                (errorMessages err)


