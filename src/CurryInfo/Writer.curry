module CurryInfo.Writer where

import CurryInfo.Types
import CurryInfo.JPretty (JPretty, json)
import CurryInfo.Paths (Path, getJSONPath)

import JSON.Pretty (ppJSON)

type Writer a b = a -> [b] -> IO ()

-- This action writes the given information into the respective json file.
writeInformation :: (Path a, JPretty b) => Writer a b
writeInformation input infos = do
    let jtext = (ppJSON . json) infos
    path <- getJSONPath input
    writeFile path jtext