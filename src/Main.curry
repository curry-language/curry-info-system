module Main ( main )
 where

import Control.Monad      ( when )
import Data.List          ( splitOn )
import Numeric            ( readHex )
import System.Directory   ( createDirectoryIfMissing )
import System.Environment ( getArgs, getEnv )
import System.Process     ( exitWith )

import CurryInfo.Commands        ( runCmd, cmdCPMUpdate )
import CurryInfo.Information     ( getInfos, printResult )
import CurryInfo.Options         ( processOptions, getObject )
import CurryInfo.Server.Server   ( startServer )
import CurryInfo.Types
import CurryInfo.Verbosity       ( printErrorMessage )

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry Package Information System (Version of 03/02/25)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  toolargs <- getArgs
  args <- if "--cgi" `elem` toolargs
            then do urlparam <- getEnv "QUERY_STRING"
                    putStrLn "Content-type: text/plain; charset=utf-8\n"
                    return $ "--cgi" :
                      map urlencoded2string (splitOn "&" urlparam)
                      -- ++ ["--force=0"]
            else return toolargs
  (opts, margs) <- processOptions banner args
  when (optUpdate opts) $ do
    let path = optCacheRoot opts
    createDirectoryIfMissing True path
    (ec,_,_) <- runCmd opts (cmdCPMUpdate opts path)
    exitWith ec
  if optServer opts
    then startServer opts
    else getObject opts >>=
         maybe (printErrorMessage "Package name is required (use --help)"
                  >> exitWith 1)
               (\obj -> do res <- getInfos opts obj margs
                           printResult opts res
                           return ())

-- From HTML.Base:
--- Translates an URL encoded string into equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string []     = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (case readHex (take 2 cs) of [(n,"")] -> n
                                                 _        -> 0)
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

