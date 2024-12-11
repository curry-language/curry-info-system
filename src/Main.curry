module Main where

import System.Environment (getArgs)
import System.Process     (exitWith)

import CurryInfo.Types
import CurryInfo.Options (processOptions, getObject)
import CurryInfo.Information (getInfos, printResult)
import CurryInfo.Server.Server (mainServer)
import CurryInfo.Server.Configuration (defaultCConfig)

main :: IO ()
main = do
  args <- getArgs
  (opts, args2) <- processOptions "" args
  if optServer opts
    then mainServer defaultCConfig (optPort opts)
    else getObject opts >>=
         maybe (putStrLn "Package name is required" >> exitWith 1)
               (\obj -> do res <- getInfos opts obj args2
                           printResult res
                           return ())
