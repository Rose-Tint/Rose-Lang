module Builder.CmdLine (
    module CmdLine,
    getCmdLine,
    getSourceFiles,
    getVerbosity,
    getBuildDir,
) where

import Control.Monad ((<$!>))

import Builder.Builder (BuilderT, getCmdLine)
import CmdLine hiding (getCmdLine)


getSourceFiles :: BuilderT m [FilePath]
{-# INLINE getSourceFiles #-}
getSourceFiles = cmdFiles <$!> getCmdLine

getVerbosity :: BuilderT m Int
{-# INLINE getVerbosity #-}
getVerbosity = cmdVerb <$!> getCmdLine

getBuildDir :: BuilderT m String
{-# INLINE getBuildDir #-}
getBuildDir = cmdBuildDir <$!> getCmdLine
