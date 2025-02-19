------------------------------------------------------------------------------
--- This modules defines an operation to read already generated information
--- from the local cache.
------------------------------------------------------------------------------

module CurryInfo.Reader where

import CurryInfo.Types
import CurryInfo.Paths     ( objectJSONPath )
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage)

import JSON.Parser         ( parseJSON )
import JSON.Data

import System.Directory    ( doesFileExist )
import System.IOExts       ( readCompleteFile )

--- This action reads the current information for the query object
--- that exist at the moment.
readObjectInformation :: Options -> QueryObject -> IO (Maybe JObject)
readObjectInformation opts obj = do
  printDebugMessage opts $
    "Determining path to JSON file of object " ++ quotePrettyObject obj ++ "..."
  let path = objectJSONPath opts obj
  printDebugMessage opts $ "Path to JSON file: " ++ path
  b <- doesFileExist path
  case b of
    False -> do
      printDebugMessage opts "JSON file does not exist.\nReading failed."
      return Nothing
    True -> do
      printDebugMessage opts "JSON file exists."
      jtext <- readCompleteFile path
      printDebugMessage opts $ "Read complete JSON file.\n" ++ jtext
      case parseJSON jtext of
        Just (JObject jobject) -> do
          printDebugMessage opts "Parsing JSON file succeeded."
          return $ Just jobject
        Nothing -> do
          printDebugMessage opts "Parsing failed."
          return Nothing
        _ -> do
          printDebugMessage opts
            "Parsing succeeded, but structure was not expected."
          return Nothing