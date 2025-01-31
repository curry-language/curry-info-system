------------------------------------------------------------------------------
--- This modules defines operations to update information in the local cache.
------------------------------------------------------------------------------

module CurryInfo.Writer ( updateObjectInformation )
 where

import Data.List           ( nubBy )

import JSON.Pretty         ( ppJSON )
import JSON.Data

import CurryInfo.Reader    ( readObjectInformation )
import CurryInfo.Types
import CurryInfo.Paths     ( getJSONPath, initializeStore )
import CurryInfo.Verbosity ( printDebugMessage, printDetailMessage )

--- This operator combines two lists and excludes all duplicates.
--- The first list should contain the newer information
--- to get an updated list.
(<+>) :: [(String, a)] -> [(String, a)] -> [(String, a)]
info1 <+> info2 = nubBy (\(k1, _) (k2, _) -> k1 == k2) (info1 ++ info2)

--- This action writes the given information about an object
--- into the respective JSON file.
writeObjectInformation :: QueryObject -> [(String, JValue)] -> IO ()
writeObjectInformation obj fields = do
  path <- getJSONPath obj
  writeFile path (ppJSON (JObject fields))

--- This action updates the JSON file of an object with the provided fields.
--- Thus, new fields are added and existing fields are replaced with the
--- new information.
updateObjectInformation :: Options -> QueryObject -> [(String, JValue)] -> IO ()
updateObjectInformation opts obj newfields = do
  path <- getJSONPath obj
  printDebugMessage opts $ "Update object in JSON file " ++ path ++ "..."
  initializeStore opts obj
  mfields <- readObjectInformation opts obj
  case mfields of
    Nothing        -> printDebugMessage opts $ errorReadingObject obj
    Just oldfields -> do
      writeFile path (ppJSON (JObject (newfields <+> oldfields)))
  printDetailMessage opts $ "JSON file " ++ path ++ " updated."
  