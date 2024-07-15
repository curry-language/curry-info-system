module CurryAnalysisInfrastructure.Interface where

import CurryAnalysisInfrastructure.JParser (JParser)
import CurryAnalysisInfrastructure.JPretty (JPretty, jsonOutput)
import CurryAnalysisInfrastructure.Configuration
import CurryAnalysisInfrastructure.Paths (Path, initialize)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Reader (Reader, readInformation)
import CurryAnalysisInfrastructure.Writer (Writer, writeInformation)
import CurryAnalysisInfrastructure.ErrorMessage (ErrorMessage, errorMessage)
import CurryAnalysisInfrastructure.Options

import JSON.Parser (parseJSON)
import JSON.Pretty (ppJSON)

import Data.Maybe (catMaybes)
import Data.List (nubBy)

-- This action extracts or generates the requested information for the given object.
getInfos :: Options -> [String] -> [String] -> IO Output
getInfos opts location requests = case location of
        [pkg]                                                   -> getInfos' opts packageConfiguration  (CurryPackage pkg) requests
        [pkg, "versions", vsn]                                  -> getInfos' opts versionConfiguration  (CurryVersion pkg vsn) requests
        [pkg, "versions", vsn, "modules", m]                    -> getInfos' opts moduleConfiguration  (CurryModule pkg vsn m) requests
        [pkg, "versions", vsn, "modules", m, "types", t]        -> return $ OutputError "getInfos for types not yet implemented!"
        [pkg, "versions", vsn, "modules", m, "typeclasses", c]  -> return $ OutputError "getInfos for types not yet implemented!"
        [pkg, "versions", vsn, "modules", m, "operations", op]  -> return $ OutputError "getInfos for types not yet implemented!"
        _ -> return $ OutputError $ show location ++ " does not match any pattern"
    where
        getInfos' :: (Path a, ErrorMessage a, EqInfo b, JParser b, JPretty b) => Options -> Configuration a b -> a -> [String] -> IO Output
        getInfos' opts conf input requests' = do
            let verb = optVerb opts
            initialize input
            result <- readInformation input
            case result of
                Nothing -> return $ OutputError $ errorMessage input
                Just infos -> do
                    results <- mapM (extractOrGenerate conf input infos) requests'
                    let newInformation = catMaybes results <+> infos
                    writeInformation input newInformation
                    return $ generateOutput requests' results

-- This action extracts or generates the requested information for the input, depending on whether the information
-- already exists or not.
extractOrGenerate :: Configuration a b -> a -> [b] -> String -> IO (Maybe b)
extractOrGenerate conf input infos request = case lookup request conf of
    Nothing                     -> return Nothing
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