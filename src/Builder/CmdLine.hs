module Builder.CmdLine (
    module Cmd,
    getSourceFiles,
) where

import Control.Monad ((<$!>))

import Builder.Internal
import Builder.CmdLine.Flags as Cmd
import Builder.CmdLine.Internal as Cmd
import Builder.CmdLine.Warnings as Cmd


getSourceFiles :: BuilderT m [FilePath]
getSourceFiles = cmdFiles . stCmdLine <$!> getState
