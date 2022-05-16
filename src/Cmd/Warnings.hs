{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Cmd.Warnings (
    Warnings(..),
    defaultWarnings,
    warningOptions,
) where

import System.Console.GetOpt


data Warnings = Warnings {
    wError :: Bool,
    nameShadowing :: Bool,
    unusedImports :: Bool,
    unusedInterns :: Bool
    }

defaultWarnings :: Warnings
defaultWarnings = Warnings {
    wError = False,
    nameShadowing = False,
    unusedImports = False,
    unusedInterns = False
    }

setWAll :: Warnings -> Warnings
setWAll w = w {
    nameShadowing = True,
    unusedImports = True,
    unusedInterns = True
    }

warningOptions :: [OptDescr (Warnings -> Warnings)]
warningOptions = [
        Option "W" ["Wall"]
            (NoArg setWAll)
            "Turns on most warnings (not all though...)",
        Option "E" ["Werror"]
            (NoArg (\w -> w { wError = True }))
            "Treat all warnings as errors",
        Option "" ["Wname-shadowing"]
            (NoArg (\w -> w { nameShadowing = True }))
            "Emit a warning when a name shadows an existing name",
        Option "" ["Wunused-imports"]
            (NoArg (\w -> w { unusedImports = True }))
            "Emit a warning if a module is imported yet unused",
        Option "" ["Wunused-interns"]
            (NoArg (\w -> w { unusedInterns = True }))
            "Emit a warning if something is 'intern'-qualified,but never used"
    ]
