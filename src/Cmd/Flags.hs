{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Cmd.Flags (Flags,
    flagOptions,
    setFlag, clrFlag,
    isFEnabled,
    f_fatal_errors,
    f_default,
) where

import Data.Bits ((.|.), testBit, bit, clearBit)
import Data.Word (Word64)
import System.Console.GetOpt


newtype Flags = Flags Word64


newtype Flag = F Int
    deriving (Show, Eq, Ord)


isFEnabled :: Flag -> Flags -> Bool
isFEnabled (F i) (Flags f) = testBit f i

setFlag, clrFlag :: Flag -> Flags -> Flags
setFlag (F i) (Flags f) = Flags (f .|. bit i)
clrFlag (F i) (Flags f) = Flags (clearBit f i)

flagOptions :: [OptDescr (Flags -> Flags)]
flagOptions = [
        Option "" ["ffatal-errors"]
            (NoArg (setFlag f_fatal_errors))
            "Stop building all modules if an error occurs",
        Option "" ["fkeep-going"]
            (NoArg (clrFlag f_fatal_errors))
            "Keep building unaffected modules, even if an error occurs"
    ]

f_default = Flags 0
f_fatal_errors = F 1
