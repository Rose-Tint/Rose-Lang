{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Cmd.Flags (
    Flags(..),
    defaultFlags,
    flagOptions,
) where

import System.Console.GetOpt


data Flags = Flags {
    fatalErrors :: Bool
    }


defaultFlags :: Flags
defaultFlags = Flags {
    fatalErrors = False
    }

flagOptions :: [OptDescr (Flags -> Flags)]
flagOptions = [
        Option "" ["ffatal-errors"]
            (NoArg (\f -> f { fatalErrors = True }))
            "Stop building all modules if an error occurs",
        Option "" ["fkeep-going"]
            (NoArg (\f -> f { fatalErrors = False }))
            "Keep building unaffected modules, even if an error occurs"
    ]
