{-# LANGUAGE FlexibleInstances #-}

module Middle.Analyzer.Error (
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
    | OtherError String
    | FalseError
    deriving (Eq)

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
        Left wrn -> pos|-"$yWarning: $R"+|wrn|+
            "\n$p"+|4.>lno|+" | $R"+|line|+
            "\n#7 $y#"+|col|+"~$r^$R\n"
        Right err -> pos|-"$rError: $R"+|err|+
            "\n$p"+|4.>lno|+" | $R"+|line|+
            "\n#7 $y#"+|col|+"~$r^$R\n"
        where
            col = srcCol pos
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
        "Type discrepency\n    Expected: "+|ex|+
                        "\n       Found: "+|fnd|+"\n"
    pretty (Undefined var []) =
        "Undefined reference to `"+|var|+"`\n"
    pretty (Undefined var simils) =
        "Undefined reference to `"+|var|+
        "`\n    Did you mean one of these?:\n"+|
        indentCatLns simils
    pretty (Redefinition orig new) =
        "Redefinition of `"-|new|-"`\n"++
        "\n    Originally defined on line "+|origLine|+
        "\n    But later defined on line "+|newLine|+"\n"
        where
            newLine = srcLine (varPos new)
            origLine = srcLine (varPos orig)
    pretty (OtherError msg) = show msg ++ "\n"
    pretty FalseError = ""
