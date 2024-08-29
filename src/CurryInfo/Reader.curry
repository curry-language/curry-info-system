module CurryInfo.Reader where

import CurryInfo.Types
import CurryInfo.JParser (JParser, jparse)
import CurryInfo.Paths (Path, getJSONPath)
import CurryInfo.Verbosity (printLine, printDebugMessage)

import JSON.Parser (parseJSON)

import System.Directory (doesFileExist)

type Reader a b = Options -> a -> IO (Maybe [b])

-- This action reads the current information for the input that exist at the moment.
readInformation :: (Path a, JParser b) => Reader a b
readInformation opts input = do
    printLine opts
    printDebugMessage opts "Determining path to json file..."
    path <- getJSONPath input
    printDebugMessage opts $ "Path to json file: " ++ path
    b <- doesFileExist path
    case b of
        False -> do
            printDebugMessage opts "json file does not exist."
            printDebugMessage opts "Reading failed."
            return Nothing
        True -> do
            printDebugMessage opts "json file exists."
            jtext <- readFile path
            printDebugMessage opts $ "Read json file.\n" ++ jtext
            case parseJSON jtext of
                Nothing -> do
                    printDebugMessage opts "Parsing json file failed."
                    return Nothing
                Just jv -> do
                    printDebugMessage opts $ "Parsed json file.\n" ++ show jv
                    return $ jparse jv