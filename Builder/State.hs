module Builder.State where

import CmdLine


data State = State {
        stCmdLine :: {-# UNPACK #-} !CmdLine,
        stFile :: !FilePath,
        stModule :: !String
    }


mkState :: CmdLine -> State
{-# INLINE mkState #-}
mkState cmd = State cmd [] []


setStateFile :: FilePath -> State -> State
{-# INLINE setStateFile #-}
setStateFile p s = s { stFile = p }
