module Builder.CmdLine (
    module Builder.CmdLine.Flags,
    module Builder.CmdLine.Internal,
    module Builder.CmdLine.Warnings,
    getSourceFiles,
    getVerbosity,
    getBuildDir,
    getCmdLine
) where

import Control.Monad ((<$!>))

import Builder.Internal
import Builder.CmdLine.Flags
import Builder.CmdLine.Internal
import Builder.CmdLine.Warnings

default (Int, Double)


getSourceFiles :: BuilderT m [FilePath]
{-# INLINE getSourceFiles #-}
getSourceFiles = cmdFiles <$!> getCmdLine

getVerbosity :: BuilderT m Int
{-# INLINE getVerbosity #-}
getVerbosity = cmdVerb <$!> getCmdLine
