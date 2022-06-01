{-# LANGUAGE FlexibleInstances #-}

module Analysis.Error (
    Error(..),
    Warning(..),
    ErrInfo(..),
    ErrMsg(..),
) where

import Data.ByteString.Lazy.Char8 (unpack)

import Builder.Internal (Stream)
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

data ErrMsg = ErrMsg [Stream] ErrInfo


instance HasSrcPos Error where
    getPos (TypeMismatch _ex fnd) = getPos fnd
    getPos (Undefined var _) = getPos var
    getPos (Redefinition _orig new) = getPos new
    getPos (BindError var typ) = var <?> typ
    getPos (InfiniteType var typ) = typ <?> var
    getPos (MissingReturn name) = getPos name
    getPos _ = UnknownPos

instance HasSrcPos Warning where
    getPos (ShadowsName _orig new) = getPos new

instance Pretty ErrMsg where
    pretty (ErrMsg lns (ErrInfo pos_ werr)) =
        header|+|srcCode
        where
            header = case werr of
                Left wrn -> "::"-|pos|-":$yWarning:$R "+|wrn
                Right err -> "::"-|pos|-": $rError:$R "+|err
            srcCode = case pos of
                UnknownPos -> ""
                _ -> "\n$p"+|5.>lno|+" | $R"+|line|+
                     "\n#8 $y#"+|col|+" $r^$R"
            pos = werr <?> pos_
            col = posCol pos - 1 :: Int
            lno = posLine pos
            line| lno < 0 = "(NEGATIVE LINE NUMBER)"
                | lno > length lns = "(EOF)"
                | otherwise = unpack (lns !! (lno - 1))


instance Pretty Warning where
    pretty (ShadowsName orig new) =
        "`"+|new|+"`<ln "+|newLine|+
        "> shadows `"
        +|orig|+"`<ln "+|origLine|+">\n"
        where
            newLine = posLine new
            origLine = posLine orig

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
            newLine = posLine new
            origLine = posLine orig
    pretty (BindError _var _t2) = "Type-Binding error"
    pretty (InfiniteType tv typ) = "Occurs Check"++
        "\n    Cannot create the infinite type `"
            +|tv|+" ~ "+|typ|+"`"
    pretty (MissingReturn name) =
        "Missing return statement in function body of `$y"
        +|name|+"$R`"
    pretty (OtherError msg) = show msg
