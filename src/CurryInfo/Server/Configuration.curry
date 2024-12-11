-----------------------------------------------------------------------------------------
--- This modules defines configuration for server mode and operations regarding that.
-----------------------------------------------------------------------------------------

module CurryInfo.Server.Configuration where

import CurryInfo.Paths (root)

import Control.Monad (when)

import System.Directory (removeFile)
import System.Process (getPID)
import System.FilePath ((</>), (<.>))

--- Debug levels intended as first parameter in debug operations below:
--- 0 : show nothing
--- 1 : show worker activity, e.g., timings
--- 2 : show server communication
--- 3 : ...and show read/store information
--- 4 : ...show also stored/computed analysis data
data DLevel = Quiet | Timing | Communicate | Storage | AllData
    deriving Enum

data CConfig = CConfig DLevel

--- This operation prints a message on the console when the given debug level
--- is greater or equal than the given integer.
debugMessage :: DLevel -> Int -> String -> IO ()
debugMessage dl n message =
  when (fromEnum dl >= n) $ putStrLn message

--- The default config used for server mode.
defaultCConfig :: CConfig
defaultCConfig = CConfig Quiet

--- This operation returns the debug level of the given config.
debugLevel :: CConfig -> DLevel
debugLevel (CConfig dl) = dl

--- This operation overwrites the debug level of the given config with the given integer.
setDebugLevel :: Int -> CConfig -> CConfig
setDebugLevel dl (CConfig _) = CConfig (toEnum dl)

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