module Thorn.Project (
    Project(..),
    Component(..),
    mkProject,
) where

import Common.Module


data Project = Proj {
    projName :: String,
    -- projOpts :: [GlobalOpt],
    projComps :: [Component]
    }

data Component
    = Exec {
        compName :: Maybe String,
        compBuildDir :: Maybe FilePath,
        compSourceDir :: Maybe FilePath,
        compModules :: [ModName]
    }
    | Lib {
        compName :: Maybe String,
        compBuildDir :: Maybe FilePath,
        compSourceDir :: Maybe FilePath,
        compModules :: [ModName]
    }


mkProject :: [FilePath] -> Project
mkProject files = if "Main.th" `elem` files then
        Proj "Main" [
            Exec (Just "main")
                Nothing
                Nothing
                (strToMod <$> files)
        ]
    else
        Proj "Main" [
            Lib (Just "main")
                Nothing
                Nothing
                (strToMod <$> files)
        ]
