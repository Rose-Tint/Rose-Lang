{-# LANGUAGE FlexibleInstances #-}

module Analysis.Error (
    Error(..),
    Warning(..),
    ErrInfo(..),
) where

import Builder.State (Stream)
import Common.SrcPos
import Common.Typing
import Common.Var
import Pretty
-- import Utils


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
        emDefName :: Maybe Var,
        emError :: Either Warning Error
    }


instance Pretty ([Stream], ErrInfo) where
    pretty (lns, (ErrInfo pos _ werr)) = case werr of
        Left wrn -> "::"-|pos|-":$yWarning: $R"+|wrn|+
            "\n$p"+|5.>lno|+" | $R"+|line|+
            "\n#8 $y#"+|col|+"~$r^$R\n"
        Right err -> "::"-|pos|-": $rError: $R"+|err|+
            "\n$p"+|5.>lno|+" | $R"+|line|+
            "\n#8 $y#"+|col|+"~$r^$R\n"
        where
            -- columns are WAY off
            col = srcCol pos - 1
            lno = srcLine pos
            line| lno < 0 = "(NEGATIVE LINE NUMBER)"
                | lno > length lns = "(EOF)"
                | otherwise = lns !! (lno - 1)

instance Pretty Warning where
    pretty (ShadowsName orig new) =
        "`"+|new|+"`<ln "+|newLine|+
        "> shadows `"
        +|orig|+"`<ln "+|origLine|+">\n"
        where
            newLine = srcLine (varPos new)
            origLine = srcLine (varPos orig)

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
            newLine = srcLine (varPos new)
            origLine = srcLine (varPos orig)
    pretty (BindError _var _t2) = "Type-Binding error"
    pretty (InfiniteType tv typ) =
        "Cannot create the infinite type `"+|tv|+" -> "+|typ|+"`"++
        "\n    Resulting from the occurence of `"+|tv|+"` in `"+|typ|+"`"
    pretty (OtherError msg) = show msg ++ "\n"
    pretty FalseError = ""
