module Main ( main )
 where

import Control.Monad      ( unless, when )
import Data.List          ( splitOn )
import Numeric            ( readHex )
import System.Environment ( getArgs, getEnv )

import Network.URL        ( urlencoded2string )
import System.Directory   ( createDirectoryIfMissing )
import System.Process     ( exitWith )

import CurryInfo.Commands        ( runCmd, cmdCPMUpdate )
import CurryInfo.Helper          ( quote )
import CurryInfo.Information     ( getAllPackageNames, getInfos, printResult )
import CurryInfo.Options         ( processOptions, getObject )
import CurryInfo.Paths           ( getReducedDirectoryContents, packagesPath )
import CurryInfo.Server.Server   ( startServer )
import CurryInfo.ToHTML          ( generateCurryInfoHTML )
import CurryInfo.Types
import CurryInfo.Verbosity       ( printDebugMessage, printDetailMessage
                                 , printErrorMessage )

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry Package Information System (Version of 06/04/25)"
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
  unless (null (optHTMLDir opts)) $ do
    generateCurryInfoHTML opts
    exitWith 0
  if optServer opts
    then startServer opts
    else getObject opts >>=
         maybe (getAllPackageNames opts >>= printResult opts >> return ())
               (\obj -> do getInfos opts obj margs >>= printResult opts
                           return ())
