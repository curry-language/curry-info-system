module CurryAnalysisInfrastructure.Reader where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.JParser (JParser, jparse)
import CurryAnalysisInfrastructure.Paths (Path, getJSONPath)
import CurryAnalysisInfrastructure.Options 

import JSON.Parser (parseJSON)

import System.Directory (doesFileExist)

import Control.Monad (when)

type Reader a b = Options -> a -> IO (Maybe [b])

-- This action reads the current information for the input that exist at the moment.
readInformation :: (Path a, JParser b) => Reader a b
readInformation opts input = do
    path <- getJSONPath input
    when (fullVerbosity opts) (putStrLn $ "path to json file: " ++ path)
    b <- doesFileExist path
    when (fullVerbosity opts) (putStrLn $ if b then "json file exists." else "json file does not exist.")
    case b of
        False -> do
            when (fullVerbosity opts) (putStrLn $ "Reading information from json file failed due to file not existing.")
            return Nothing
        True -> do
            jtext <- readFile path
            when (fullVerbosity opts) (putStrLn $ "Read json file.\n" ++ jtext)
            let jv = parseJSON jtext
            case parseJSON jtext of
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Parsing json file failed.")
                    return Nothing
                Just jv -> do
                    when (fullVerbosity opts) (putStrLn $ "Parsed json file.\n" ++ show jv)
                    return $ jparse jv