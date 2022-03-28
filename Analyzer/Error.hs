module Analyzer.Error where

import Data.Text (Text, unpack)

import CmdLine (CmdLine(cmdTermWidth))
import Color
import Pretty
import Parser.Data hiding (Type)
import SymbolTable.SymbolData
import Typing.Types
import Utils


default (Int, Double)



data Error
    = TypeMismatch Type Type
    | Undefined Symbol [Symbol]
    | OtherError String
    | FalseError
    deriving (Show, Eq)


data Warning
    = ShadowsName Symbol Symbol
    deriving (Show, Eq)


data ErrorMessage
    = ErrorMessage {
        emPosition :: Position,
        emDefName :: Maybe Symbol,
        emError :: Either Warning Error
    }
    deriving (Show)



prettyError :: CmdLine -> [Text] -> ErrorMessage -> String
prettyError cmd lns em = printf
    "%s\n%s\n       $R%s$r%s$R\n"
    (pretty em) src caretStart caretRed
    where
        pos = emPosition em
        lno = min (length lns) (max 0 (posLine pos))
        -- "..." + "%5d | " = 11 wide
        width = cmdTermWidth cmd
        width' = width - 8
        width'' = width' - 3
        mainLine = if length str < width' then str else
            if posEnd pos >= width' then
                "..." ++ drop (min
                    (width'')
                    (posEnd pos - width')
                ) str
            else take width'' str ++ "..."
            where
                str = unpack $! lns !! (lno - 1)
        caretStart = replicate (max 0 (posStart pos)) ' '
        caretRed = replicate (posEnd pos - posStart pos) '^'
        src | lno <= 0 = "   0 | " ++ unpack (head lns)
            | otherwise = printf "$b%5d | $R%s\n$b%5d | $R"
                    (lno - 1) (unpack (lns !! (lno - 2)))
                    lno ++ mainLine



instance Pretty ErrorMessage where
    pretty em = printf
        -- "%s\n    Position: %s\n    Location: %s\n"
        -- err pos def
        "%s: %s\n    Position: Line %d, Column %d (to %d?)"
            (posModule pos) err (posLine pos)
            (posStart pos) (posEnd pos)
        where
            pos = emPosition em
            -- def = case emDefName em of
            --     Nothing -> "most likely at the top level"
            --     Just name -> "in the definition of " ++ detailed name
            err = case emError em of
                Left (ShadowsName new orig) -> printf
                    "$yWarning:$R `%s` shadows existing binding `%s`"
                        (pretty new) (pretty orig)
                Right (TypeMismatch got ex) -> printf
                    "$rError:$y Type mismatch\n\
                    \    $rExpected: $R%s\n\
                    \       $rFound: $R%s"
                    (pretty got) (pretty ex)
                Right (Undefined sym syms) -> printf
                    "$rError:$y Undefined reference to `$R%s$y`\n\
                    \    Possible replacements:\n%s"
                    (pretty sym) (indentAllUsing pretty syms)
                Right (OtherError msg) -> printf
                    "$rError:$y Other error?\n    $R%s\n" msg
                _ -> "_"
