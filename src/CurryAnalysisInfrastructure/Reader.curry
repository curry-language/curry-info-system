module CurryAnalysisInfrastructure.Reader where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.JParser (JParser, jparse)
import CurryAnalysisInfrastructure.Paths (Path, getJSONPath)

import JSON.Parser (parseJSON)

import System.Directory (doesFileExist)

type Reader a b = a -> IO (Maybe [b])

-- This action reads the current information for the input that exist at the moment.
readInformation :: (Path a, JParser b) => Reader a b
readInformation input = do
    path <- getJSONPath input
    b <- doesFileExist path
    case b of
        False -> return Nothing
        True -> do
            jtext <- readFile path
            return $ parseJSON jtext >>= jparse