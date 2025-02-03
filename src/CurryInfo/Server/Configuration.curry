------------------------------------------------------------------------------
--- This modules defines the configuration for server mode and operations
--- on the configuration.
------------------------------------------------------------------------------

module CurryInfo.Server.Configuration where

import Control.Monad (when)

import System.Directory (removeFile)
import System.Process   (getPID)
import System.FilePath  ((</>), (<.>))
import System.IOExts    (readCompleteFile)

import CurryInfo.Types  ( Options(..) )

--- This action returns the path to the server port file.
serverPortFileName :: Options -> FilePath
serverPortFileName opts = optCacheRoot opts </> ".curryinfo.port"

--- This action stores the given port in the server port file.
storeServerPortNumber :: Options -> Int -> IO ()
storeServerPortNumber opts portnum = do
  mypid <- getPID
  writeFile (serverPortFileName opts) (show (portnum, mypid))

--- This action removes the server port file.
removeServerPortNumber :: Options -> IO ()
removeServerPortNumber opts = removeFile (serverPortFileName opts)

--- This action reads the currently stored port from the server port file.
readServerPortPid :: Options -> IO (Int, Int)
readServerPortPid opts =
  readCompleteFile (serverPortFileName opts) >>= return . read

--- The wait time for accepting sockets.
waitTime :: Int
waitTime = -1