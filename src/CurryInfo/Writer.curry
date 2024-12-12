-----------------------------------------------------------------------------------------
--- This modules defines operations to write information into the local cache.
-----------------------------------------------------------------------------------------

module CurryInfo.Writer where

import CurryInfo.Types
import CurryInfo.Paths (getJSONPath)

import JSON.Pretty (ppJSON)
import JSON.Data

import Data.List (nubBy)

--- This operator combines two lists and excludes all duplicates.
--- The first list should contain the newer information
--- to get an updated list.
(<+>) :: [(String, a)] -> [(String, a)] -> [(String, a)]
info1 <+> info2 = nubBy (\(k1, _) (k2, _) -> k1 == k2) (info1 ++ info2)

--- This action writes the given information about an object
--- into the respective json file.
writeObjectInformation :: QueryObject -> [(String, JValue)] -> IO ()
writeObjectInformation obj fields = do
    let jtext = (ppJSON . JObject) fields
    path <- getJSONPath obj
    writeFile path jtext