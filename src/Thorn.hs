{-# LANGUAGE LambdaCase #-}

module Thorn (
    runThorn,
    buildWithThorn,
) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)

import Builder
import Cmd
import Common.Module
import Compiler
import Text.Pretty
import Thorn.Parser
import Thorn.Project


runThorn :: CmdLine -> Project -> IO ()
runThorn cmd proj = runBuilder (buildProject proj) cmd

buildWithThorn :: CmdLine -> IO ()
buildWithThorn = runBuilder
    (readBuildFile >>= buildProject)

findBuildFile :: Builder FilePath
findBuildFile = asks cmdFiles >>= \case
    [] -> do
        isInCurr <- io (doesFileExist "./thorn.build")
        if isInCurr then do
            modify $ \s -> s { sourceDirectory = "./" }
            return "./thorn.build"
        else
            fatal $ "Could not find 'thorn.build'.\n"++
                "Consider passing its parent directory"++
                " to the command line"
    (dir:_) -> do
        let path = dir ++ "/thorn.build"
        exists <- io (doesFileExist path)
        if exists then do
            -- save dir for `buildProject` and `buildComp`
            modify $ \s -> s { sourceDirectory = dir }
            return path
        else
            fatal $ "Could not find a file named "++
                " 'thorn.build.' in the provided directory."

readBuildFile :: Builder Project
readBuildFile = do
    path <- findBuildFile
    src <- io (BS.readFile path)
    case runAlex src thornP of
        Left msg -> fatal (strToMod path|+"::"+|msg|+"\n")
        Right proj -> return proj

buildProject :: Project -> Builder ()
buildProject (Proj name comps) = do
    message ("Building project: "+|name|+"\n")
    mapM_ buildComp comps

buildComp :: Component -> Builder ()
buildComp comp = do
    let name = compName comp
    message ("Building component: "+|name|+"\n")
    base <- gets sourceDirectory
    let cbd = compBuildDir comp
        csd = compSourceDir comp
    modify $ \s -> s {
        buildDirectory = fromMaybe base cbd,
        sourceDirectory = fromMaybe base csd
        }
    let modules = compModules comp
    compile (fmap (`modToFile` ".th") modules)
