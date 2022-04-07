module Builder.CmdLine (
    module CmdLine,
    getCmdLine,
    getSourceFiles,
    getVerbosity,
    getBuildDir,
) where

import Control.Monad ((<$!>))

import Builder.Builder
import CmdLine hiding (getCmdLine)


getSourceFiles :: BuilderT m [FilePath]
{-# INLINE getSourceFiles #-}
getSourceFiles = cmdFiles <$!> getCmdLine

getVerbosity :: BuilderT m Int
{-# INLINE getVerbosity #-}
getVerbosity = cmdVerb <$!> getCmdLine
