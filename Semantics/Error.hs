module Semantics.Error where

import Color
import Parser.Data



data Error
    = TypeMismatch Type Type
    | OutOfScope String
    | ManyErrors [Error]
    | TooManyArgs Type [Value]
    | OtherError String
    | UnknownError
    deriving (Show, Eq)



-- Keep errors in order!
mergeErrors :: Error -> Error -> Error
mergeErrors (ManyErrors es1) (ManyErrors es2)
    = ManyErrors (es1 ++ es2)
mergeErrors (ManyErrors es) e = ManyErrors (es ++ [e])
mergeErrors e (ManyErrors es) = ManyErrors (e:es)
mergeErrors e1 e2 = ManyErrors [e1, e2]


prettyError :: String -> Error -> String
prettyError modName (TypeMismatch t1 t2) = printf
    "%s:$rError: Mismatching types:\n\
    \    $yExpected Type: $r%s\n\
    \      $yActual Type: $r%s"
    modName (show t1) (show t2)
prettyError modName (OutOfScope iden) = printf
    "%s:$rError: Identifier not in scope: $y%s\n"
    modName iden
prettyError _ _ = ""
