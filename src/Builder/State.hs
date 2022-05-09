module Builder.State (
    Stream,
    State(..),
    mkState,
) where

import Data.Set (Set, empty)

import Builder.CmdLine.Internal


type Stream = String

data State = State {
        stCmdLine :: CmdLine,
        stFile :: FilePath,
        stModule :: String,
        -- current build directory, as opposed to
        -- cmdBuildDir, which is the base
        stBuildDir :: FilePath,
        -- list of visited files
        stVisited :: Set FilePath,
        stSource :: String
    }


mkState :: CmdLine -> State
{-# INLINE mkState #-}
mkState cmd = State cmd [] [] [] empty ""

 :: 
