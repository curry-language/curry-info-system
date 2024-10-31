-----------------------------------------------------------------------------------------
--- This modules defines an operation to read already generated information from the local cache.
-----------------------------------------------------------------------------------------

module CurryInfo.Reader where

import CurryInfo.Types
import CurryInfo.Paths (Path, getJSONPath)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)

import JSON.Parser (parseJSON)
import JSON.Data

import System.Directory (doesFileExist)

-- This action reads the current information for the input that exist at the moment.
readInformation :: Path a => Options -> a -> IO (Maybe [(String, JValue)])
readInformation opts obj = do
    printDebugMessage opts "Detemining path to json file..."
    path <- getJSONPath obj
    printDebugMessage opts $ "Path to json file: " ++ path
    b <- doesFileExist path
    case b of
        False -> do
            printDebugMessage opts "json file does not exist.\nReading failed."
            return Nothing
        True -> do
            printDebugMessage opts "json file exists."
            jtext <- readFile path
            printDebugMessage opts $ "Read json file.\n" ++ jtext
            case parseJSON jtext of
                Just (JObject fields) -> do
                    printDebugMessage opts "Parsing json file succeeded."
                    return $ Just fields
                Nothing -> do
                    printDebugMessage opts "Parsing failed."
                    return Nothing
                _ -> do
                    printDebugMessage opts "Parsing succeeded, but structure was not expected."
                    return Nothing