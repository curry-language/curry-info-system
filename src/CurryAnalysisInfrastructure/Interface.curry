module CurryAnalysisInfrastructure.Interface where

import CurryAnalysisInfrastructure.JParser (JParser)
import CurryAnalysisInfrastructure.JPretty (JPretty, jsonOutput)
import CurryAnalysisInfrastructure.Configuration
import CurryAnalysisInfrastructure.Paths (Path, initialize)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Reader (Reader, readInformation)
import CurryAnalysisInfrastructure.Writer (Writer, writeInformation)
import CurryAnalysisInfrastructure.ErrorMessage (ErrorMessage, errorMessage)

import JSON.Parser (parseJSON)
import JSON.Pretty (ppJSON)

import Data.Maybe (catMaybes)
import Data.List (nubBy)

-- This action extracts or generates the requested information for the given object.
getInfos :: [String] -> [String] -> IO Output
getInfos location requests = case location of
        [pkg] -> getInfos'   packageConfiguration  (CurryPackage pkg) requests
        [pkg, "versions", vsn] -> getInfos'   versionConfiguration  (CurryVersion pkg vsn) requests
        [pkg, "versions", vsn, "modules", m] -> getInfos'   moduleConfiguration  (CurryModule pkg vsn m) requests
        _ -> return $ OutputError $ show location ++ " does not match any pattern"
    where
        getInfos' :: (Path a, ErrorMessage a, EqInfo b, JParser b, JPretty b) => Configuration a b -> a -> [String] -> IO Output
        getInfos' conf input requests' = do
            initialize input
            result <- readInformation input
            case result of
                Nothing -> return $ OutputError $ errorMessage input
                Just infos -> do
                    results <- mapM (extractOrGenerate conf input infos) requests'
                    let newInformation = catMaybes results <+> infos
                    writeInformation input newInformation
                    return $ generateOutput requests' results
        {-
        getInfos' :: (Path a, JParser b, JPretty b) => Reader a b -> ErrorMessage a -> Configuration a b -> Writer a b -> a -> [String] -> IO Output
        getInfos' reader errorMessage conf writer input requests = do
            result <- reader input
            case result of
                Nothing -> return $ OutputError $ errorMessage input
                Just infos -> do
                    results <- mapM (extractOrGenerate conf input infos) requests
                    let newInformation = mixInformation infos (catMaybes results)
                    writer input newInformation
                    return $ generateOutput requests results
        -}

-- This action extracts or generates the requested information for the input, depending on whether the information
-- already exists or not.
extractOrGenerate :: Configuration a b -> a -> [b] -> String -> IO (Maybe b)
extractOrGenerate conf input infos request = case lookup request conf of
    Nothing -> return Nothing
    Just (extractor, generator) -> maybe (generator input) (return . Just) (extractor infos)

-- This function generates an output for the fields and respective results of extracting or generating.
generateOutput :: JPretty a => [String] -> [Maybe a] -> Output
generateOutput fields results =
    let outputs = zipWith (\f r -> maybe (f, "failed") jsonOutput r) fields results
    in OutputText $ unlines $ map (\(f, m) -> f ++ ": " ++ m) outputs

-- This operator combines two lists and excludes all dublicates. The first list should contain the newer information
-- to get an updated list.
(<+>) :: EqInfo a => [a] -> [a] -> [a]
info1 <+> info2 = nubBy sameInfo (info1 ++ info2)