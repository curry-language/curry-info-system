module CurryAnalysisInfrastructure.Interface where

import CurryAnalysisInfrastructure.JParser
import CurryAnalysisInfrastructure.JPretty
import CurryAnalysisInfrastructure.Configuration
import CurryAnalysisInfrastructure.Paths
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Reader
import CurryAnalysisInfrastructure.Writer
import CurryAnalysisInfrastructure.ErrorMessage

import JSON.Parser (parseJSON)
import JSON.Data
import JSON.Pretty (ppJSON)

import System.Directory

import Data.Maybe (catMaybes)
import Data.List (nubBy)

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

extractOrGenerate :: Configuration a b -> a -> [b] -> String -> IO (Maybe b)
extractOrGenerate conf input infos request = case lookup request conf of
    Nothing -> return Nothing
    Just (extractor, generator) -> maybe (generator input) (return . Just) (extractor infos)

generateOutput :: JPretty a => [String] -> [Maybe a] -> Output
generateOutput fields results =
    let outputs = zipWith (\f r -> maybe (f, "failed") jsonOutput r) fields results
    in OutputText $ unlines $ map (\(f, m) -> f ++ ": " ++ m) outputs

(<+>) :: EqInfo a => [a] -> [a] -> [a]
info1 <+> info2 = nubBy sameInfo (info1 ++ info2)