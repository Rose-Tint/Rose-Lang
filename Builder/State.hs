module Builder.State (
    Stream,
    State(..),
    mkState,
) where

import Data.Set as S (Set, empty)
import Data.Text as T (Text, empty)

import CmdLine


type Stream = Text

data State = State {
        stCmdLine :: {-# UNPACK #-} !CmdLine,
        stFile :: !FilePath,
        stModule :: !String,
        -- current build directory, as opposed to
        -- cmdBuildDir, which is the base
        stBuildDir :: !FilePath,
        -- list of Up-To-Date modules, whether it's
        -- because they were already built, or because
        -- (in the future) they dont need to be
        stUTDModules :: Set String,
        stSource :: Text
    }


mkState :: CmdLine -> State
{-# INLINE mkState #-}
mkState cmd = State cmd [] [] [] S.empty T.empty
