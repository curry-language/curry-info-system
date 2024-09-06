module CurryInfo.Writer where

import CurryInfo.Types
import CurryInfo.Paths (Path, getJSONPath)

import JSON.Pretty (ppJSON)
import JSON.Data

type Writer a b = a -> [b] -> IO ()

-- This action writes the given information into the respective json file.
writeInformation :: Path a => a -> [(String, JValue)] -> IO ()
writeInformation obj fields = do
    let jtext = (ppJSON . JObject) fields
    path <- getJSONPath obj
    writeFile path jtext