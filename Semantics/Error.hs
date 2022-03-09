module Semantics.Error where

import qualified Data.Text as T
    (Text, splitAt)

import Color
import Parser.Data
import Parser.Pretty



data Error
    = TypeMismatch Variable Type Type
    | Redefinition Variable Variable
    | OutOfScope Variable
    | IndetType Variable
    | TooManyArgs Variable Type [Value]
    | TooManyParams Variable Type [Variable]
    | ManyErrors [Error]
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


highlightVar :: [T.Text] -> Variable -> String
highlightVar src (Var _ ln begin end) = printf
    "$R%4d | %s$r%s$R%s"
    ln preVar name postVar
    where
        srcLine = src !! (max 0 (ln - 1))
        varLen = end - begin
        (preVar, srcPostStart) = T.splitAt (begin - 1) srcLine
        (name, postVar) = T.splitAt varLen srcPostStart


prettyError :: [T.Text] -> String -> Error -> String
prettyError src modName (TypeMismatch var t1 t2) = printf
    "$rError in [%s]: $yMismatching types:$y\n\
    \    Expected Type: `$r%s$y`\n\
    \      Actual Type: `$r%s$y`\n\n"
    modName (prettyType t1) (prettyType t2)
    (highlightVar src var)
prettyError src modName (Redefinition orig new) = printf
    "$rError in [%s]: $yRedefinition of identifier:$y\n\
    \    First defined here: \n%s\n\
    \    Later defined here: \n%s\n\n"
    modName (highlightVar src orig) (highlightVar src new)
prettyError src modName (OutOfScope var) = printf
    "$rError in [%s]: $yIdentifier not in scope:\n\
    \    Identifier: %s\n\
    \%s\n\n"
    modName (varName var) (highlightVar src var)
prettyError src modName (ManyErrors es) =
    concat $! fmap (prettyError src modName) es
prettyError src modName (IndetType var) = printf
    "$rError in [%s]: $yCould not determine type from context:\n%s\n\n"
    modName (highlightVar src var)
prettyError src modName (TooManyArgs func typ args) = printf
    "$rError in [%s]: $yToo many arguments in function application:$y\n\
    \    %s expected %d argument(s), but was given %d.\n\
    \    %s's type: %s\n%s\n\n"
    modName (varName func)
    (case typ of TerminalType _ _ -> 0
                 NonTermType l -> length l)
    (length args) (prettyType typ) (highlightVar src func)
prettyError src modName (TooManyParams func typ pars) = printf
    "$rError in [%s]: $yToo many parameters in function definition:$y\n\
    \    %s takes at most %d parameters, but was given %d.\n\
    \    %s's type: %s\n%s\n\n"
    modName (varName func)
    (case typ of TerminalType _ _ -> 0
                 NonTermType l -> length l)
    (length pars) (prettyType typ) (highlightVar src func)
prettyError _ modName (OtherError msg) = printf
    "$rError in [%s]: $yOther error:\n    $y%s\n\n"
    modName msg
prettyError _ _ UnknownError = ""
-- prettyError _ modName UnknownError = printf
    -- "Error in [%s]: Unknown error?\n" modName
