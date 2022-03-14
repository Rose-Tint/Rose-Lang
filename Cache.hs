module Cache where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad ((<$!>))
import Data.List (isPrefixOf, stripPrefix)
import Data.Time
import System.Directory
import System.IO

import CmdLine
import Utils



isUpToDate :: CmdLine -> FilePath -> IO Bool
isUpToDate cmd p = do
    let modName = pathToModule p
    let modNamePrefix = modName ++ "::"
    let cachePath = cmdBuildDir cmd ++ "/Cache/Mod-Dates"
    lastMod <- getModificationTime p
    file <- openFile cachePath ReadMode
    contents <- lines <$!> hGetContents file
    hClose file
    return $! foldr (\line b ->
        case stripPrefix modNamePrefix line of
            Nothing -> b
            Just rest ->
                let time = read rest :: UTCTime
                in time >= lastMod
        ) False contents

cacheBuildTime :: CmdLine -> FilePath -> IO ThreadId
cacheBuildTime cmd p = forkIO $! do
    let modName = pathToModule p
    let cachePath = cmdBuildDir cmd
            ++ "/Cache/Mod-Dates"
    lastMod <- getModificationTime p
    file <- openFile cachePath ReadWriteMode
    contents <- lines <$!> hGetContents file
    let newContents = foldr (\line cntnts -> cntnts ++
            if modName `isPrefixOf` line then
                modName ++ "::" ++ show lastMod ++ "\n"
            else
                line ++ "\n"
            ) "" contents
    hClose file
    writeFile cachePath newContents
