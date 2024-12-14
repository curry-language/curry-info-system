-----------------------------------------------------------------------------------------
--- This modules defines configuration for server mode and operations regarding that.
-----------------------------------------------------------------------------------------

module CurryInfo.Server.Configuration where

import CurryInfo.Paths (root)

import Control.Monad (when)

import System.Directory (removeFile)
import System.Process (getPID)
import System.FilePath ((</>), (<.>))

--- This action returns the path to the server port file.
getServerPortFileName :: IO String
getServerPortFileName = do
  path <- root
  return (path </> ".curryinfo.port")

--- This action stores the given port in the server port file.
storeServerPortNumber :: Int -> IO ()
storeServerPortNumber portnum = do
  mypid <- getPID
  serverPortFileName <- getServerPortFileName
  writeFile serverPortFileName (show (portnum, mypid))

--- This action removes the server port file.
removeServerPortNumber :: IO ()
removeServerPortNumber = getServerPortFileName >>= removeFile

--- This action reads the currently stored port from the server port file.
readServerPortPid :: IO (Int, Int)
readServerPortPid = getServerPortFileName >>= readFile >>= return . read

--- The wait time for accepting sockets.
waitTime :: Int
waitTime = -1