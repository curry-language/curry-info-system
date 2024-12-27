module Main where

import Data.List          ( splitOn )
import System.Environment ( getArgs, getEnv )
import System.Process     ( exitWith )

import CurryInfo.Types
import CurryInfo.Options         ( processOptions, getObject )
import CurryInfo.Information     ( getInfos, printResult )
import CurryInfo.Server.Server   ( startServer )

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry Package Information System (Version of 27/12/24)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  toolargs <- getArgs
  args <- if "--cgi" `elem` toolargs
            then do urlparam <- getEnv "QUERY_STRING"
                    putStrLn "Content-type: text/plain; charset=utf-8\n"
                    return $ "--cgi" : splitOn "&" urlparam ++ ["--force=0"]
            else return toolargs
  (opts, margs) <- processOptions banner args
  if optServer opts
    then startServer opts
    else getObject opts >>=
         maybe (putStrLn "Package name is required (use --help)" >> exitWith 1)
               (\obj -> do res <- getInfos opts obj margs
                           printResult opts res
                           return ())
