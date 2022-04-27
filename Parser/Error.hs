module Parser.Error (prettyParseErr) where

import Prelude hiding (lines)

import Data.Text (Text, lines)

import Pretty


default (Int, Double)


prettyParseErr :: ParseError -> Text -> String
prettyParseErr err input = 
    if errLn < length srcLines then
        Red|+"Error while parsing "+|pos|+":\n"+|
        Purple|+|4.>posLine|+" | "+|Reset|+|srcLine|+"\n"+|
        replicate (posCol + 8) ' '|+|Red|+"^"+|
        Reset|+|errMsg|+"\n"
    else
        Red|+"unexpected EOF resulting from:\n    "+|errMsg|+"\n"
    where
        pos = errorPos err
        srcLines = lines input
        posLine = sourceLine pos
        posCol = sourceColumn pos
        errLn = posLine - 1
        srcLine = lines input !! (posLine - 1)
        errMsg = showErrorMessages "or" "unknown"
                "Expected:" "Found: " "end of input"
                (errorMessages err)


