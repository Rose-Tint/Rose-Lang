{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Cmd.Warnings (Warning,
    warningOptions,
    isWEnabledFor,
    enableWarningFor,
    w_default,
    w_error,
    w_name_shadowing,
    w_unused_imports,
    w_unused_interns,
) where

import Data.Bits
import Data.Int (Int64)
import System.Console.GetOpt

default (Int64)


newtype Warning = W Int64
    deriving (Show, Eq, Ord)


warningOptions :: [OptDescr Warning]
warningOptions = [
        Option "W" ["Wall"]           (NoArg w_all)
            "Turns on most warnings (not all though...)",
        Option "E" ["Werror"]         (NoArg w_error)
            "Treat all warnings as errors",
        Option "" ["Wname-shadowing"] (NoArg w_name_shadowing)
            "Emit a warning when a name shadows an existing name",
        Option "" ["Wunused-imports"] (NoArg w_unused_imports)
            "Emit a warning if a module is imported yet unused",
        Option "" ["Wunused-interns"] (NoArg w_unused_interns)
            "Emit a warning if something is 'intern'-qualified,\
                \but never used"
    ]

isWEnabledFor :: Warning -> Warning -> Bool
isWEnabledFor (W w1) (W w2) = w1 .&. w2 /= 0

enableWarningFor :: Warning -> Warning -> Warning
enableWarningFor (W w1) (W w2) = W (w1 .|. w2)

w :: Int64 -> Warning
w = W . (^(2 :: Int64))
w_default = foldr enableWarningFor (W 0) [
        w_name_shadowing,
        w_unused_imports
    ]
w_all = foldr enableWarningFor w_default [
        w_name_shadowing,
        w_unused_imports
    ]
w_error = w 1
w_name_shadowing = w 2
w_unused_imports = w 3
w_unused_interns = w 4
