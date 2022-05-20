{-# LANGUAGE FlexibleInstances #-}

module Analysis.Error (
    Error(..),
    Warning(..),
    ErrInfo(..),
) where

import Common.SrcPos
import Common.Var
import Text.Pretty
import Typing.Type


default (Int, Double)


data Error
    = TypeMismatch
        Type -- expected
        Type -- found
    | Undefined
        Var -- name
        [Var] -- similar names
    | Redefinition
        Var -- original
        Var -- new
    | BindError Var Type
    | InfiniteType Var Type
    | MissingReturn Var -- name of function
    | OtherError String
    | FalseError
    -- deriving (Eq)

data Warning
    = ShadowsName
        Var -- original
        Var -- new
    deriving (Eq)

data ErrInfo
    = ErrInfo {
        emPos :: SrcPos,
        emError :: Either Warning Error
    }


fmtSrcLns :: Int -> Int -> String -> String
fmtSrcLns st en = concatMap (\(lno, line) ->
    "\n$p"+|5.>lno|+" | $R"+|line
    ) . zip [st..en] . lines

instance HasSrcPos Error where
    getPos (TypeMismatch _ex fnd) = getPos fnd
    getPos (Undefined var _) = getPos var
    getPos (Redefinition _orig new) = getPos new
    getPos (BindError var typ) = var <?> typ
    getPos (InfiniteType var typ) = typ <?> var
    getPos (MissingReturn name) = getPos name
    getPos _ = UnknownPos

instance Pretty ([String], ErrInfo) where
    pretty (_, (ErrInfo pos@UnknownPos werr)) = case werr of
        Left wrn -> "::"-|pos|-":$yWarning: $R\n    "+|wrn|+"\n"
        Right err -> "::"-|pos|-": $rError: $R\n    "+|err|+"\n"
    pretty (lns, (ErrInfo pos_ werr)) = case werr of
        Left wrn -> "::"-|pos|-":$yWarning: $R\n    "+|wrn|+
            fmtSrcLns stLn endLn (getCodeAsRed pos lns)|+
            "\n#8 $y#"+|col|+" $r#"+|width|+"^$R\n"
        Right err -> "::"-|pos|-": $rError: $R\n    "+|err|+
            fmtSrcLns stLn endLn (getCodeAsRed pos lns)|+
            "\n#8 $y#"+|col|+" $r#"+|width|+"^$R\n"
        where
            pos = either (const pos_) (<?> pos_) werr
            stLn = posStartLine pos
            endLn = posEndLine pos
            col = posStartCol pos
            width = calcWidth pos

instance Pretty Warning where
    pretty (ShadowsName orig new) =
        "`"+|new|+"`<ln "+|newLine|+
        "> shadows `"
        +|orig|+"`<ln "+|origLine|+">\n"
        where
            newLine = posStartLine new
            origLine = posStartLine orig

instance Pretty Error where
    pretty (TypeMismatch ex fnd) =
        "Type discrepency$R\n    Expected: "+|ex|+
                          "\n       Found: "+|fnd
    pretty (Undefined var []) =
        "Undefined reference to `$y"+|var|+"$R`"
    pretty (Undefined var simils) =
        "Undefined reference to `$y"+|var|+
        "$R`\n    Did you mean one of these?:\n        '$y"
            +|"$R`, `$y"`sepsD`simils|+"$R'"
    pretty (Redefinition orig new) =
        "Redefinition of `$y"-|new|-"$R`\n"++
        "\n    Originally defined on line "+|origLine|+
        "\n    But later defined on line "+|newLine
        where
            newLine = posStartLine new
            origLine = posStartLine orig
    pretty (BindError _var _t2) = "Type-Binding error"
    pretty (InfiniteType tv typ) =
        "Cannot create the infinite type `"+|tv|+" ~> "+|typ|+"`"
    pretty (MissingReturn name) =
        "Missing return statement in function body of `$y"
        +|name|+"$R`"
    pretty (OtherError msg) = show msg
    pretty FalseError = ""
