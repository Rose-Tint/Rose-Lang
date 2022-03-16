module Main (main) where

import Data.Time

import Build
import CmdLine



main :: IO ()
main = do
    let iters = 500 :: Integer
    (mn, total, mx) <- go iters
    putStrLn $! "Ran " ++ show iters
        ++ " times. Here are the results:\n    Avg : "
        ++ show (total / fromInteger iters)
        ++ "\n    Min : " ++ show mn
        ++ "\n    Max : " ++ show mx
    where
        go 0 = return (100000, 0, 0)
        go c = do
            (mn, total, mx) <- go $! c - 1
            this <- main'
            let total' = total + this
                mn' = min mn this
                mx' = max mx this
            return (mn', total', mx')

main' :: IO NominalDiffTime
main' = do
    cmdLine' <- getCmdLine
    let cmdLine = cmdLine' {
                cmdFiles = [
                        "Std/Data/Tree.th",
                        "Std/Memory.th",
                        "Std/Compare.th",
                        "Std/Maybe.th",
                        "Std/Bit.th",
                        "Std/Hash.th"
                    ],
                cmdVerb = 0,
                cmdTrace = False
            }
    buildStart <- getCurrentTime
    build cmdLine
    buildEnd <- getCurrentTime
    return $! diffUTCTime buildEnd buildStart
