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

debugMessage :: DLevel -> Int -> String -> IO ()
debugMessage dl n message = debugString dl n (message ++ "\n")

debugString :: DLevel -> Int -> String -> IO ()
debugString dl n message = when (fromEnum dl >= n) $ putStr message

data CConfig = CConfig DLevel

defaultCConfig :: CConfig
defaultCConfig = CConfig Quiet

debugLevel :: CConfig -> DLevel
debugLevel (CConfig dl) = dl

setDebugLevel :: Int -> CConfig -> CConfig
setDebugLevel dl (CConfig _) = CConfig (toEnum dl)

getServerPortFileName :: IO String
getServerPortFileName = do
    path <- root
    return (path </> ".curryinfo.port")

storeServerPortNumber :: Int -> IO ()
storeServerPortNumber portnum = do
    mypid <- getPID
    serverPortFileName <- getServerPortFileName
    writeFile serverPortFileName (show (portnum, mypid))

removeServerPortNumber :: IO ()
removeServerPortNumber = getServerPortFileName >>= removeFile

readServerPortPid :: IO (Int, Int)
readServerPortPid = getServerPortFileName >>= readFile >>= return . read

waitTime :: Int
waitTime = -1