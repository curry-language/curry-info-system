module Main where

import System.Environment (getArgs)
import System.Process     (exitWith)

import CurryInfo.Types
import CurryInfo.Options (processOptions, getObject)
import CurryInfo.Information (getInfos, printResult)
import CurryInfo.Server.Server (mainServer)
import CurryInfo.Server.Configuration (defaultCConfig)

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry Package Information System (Version of 13/12/24)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  (opts, args) <- getArgs >>= processOptions banner
  if optServer opts
    then mainServer defaultCConfig (optPort opts)
    else getObject opts >>=
         maybe (putStrLn "Package name is required" >> exitWith 1)
               (\obj -> do res <- getInfos opts obj args
                           printResult res
                           return ())
