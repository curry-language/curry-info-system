module Main where

import CurryAnalysisInfrastructure.Options (Options (..), processOptions)

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

import Data.Maybe (catMaybes, isJust)
import Data.List (nubBy)

import System.Environment (getArgs)
import System.Process (exitWith)

import Control.Monad (unless, when)

-- This action extracts or generates the requested information for the given object.
getInfos :: Options -> [(String, String)] -> [String] -> IO Output
getInfos opts location requests = case location of
        [("packages", pkg)]                                                         -> getInfos' opts packageConfiguration  (CurryPackage pkg) requests
        [("packages", pkg), ("versions", vsn)]                                      -> getInfos' opts versionConfiguration  (CurryVersion pkg vsn) requests
        [("packages", pkg), ("versions", vsn), ("modules", m)]                      -> getInfos' opts moduleConfiguration  (CurryModule pkg vsn m) requests
        [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)]        -> return $ OutputError "getInfos for types not yet implemented!"
        [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)]  -> return $ OutputError "getInfos for types not yet implemented!"
        [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", op)]  -> return $ OutputError "getInfos for types not yet implemented!"
        _ -> return $ OutputError $ show location ++ " does not match any pattern"
    where
        getInfos' :: (Path a, ErrorMessage a, EqInfo b, JParser b, JPretty b) => Options -> Configuration a b -> a -> [String] -> IO Output
        getInfos' opts conf input requests' = do
            when (fullVerbosity opts) (putStrLn "Initializing Input...")
            initialize input
            when (fullVerbosity opts) (putStrLn "Reading current information...")
            result <- readInformation input
            case result of
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn "Reading information failed.")
                    return $ OutputError $ errorMessage input
                Just infos -> do
                    when (fullVerbosity opts) (putStrLn "Reading information succeeded.")
                    when (fullVerbosity opts) (putStrLn "Extracting/Generating requested information...")
                    results <- mapM (extractOrGenerate opts conf input infos) requests'

                    let newInformation = catMaybes results <+> infos
                    when (fullVerbosity opts) (putStrLn "Overwriting with updated information")
                    writeInformation input newInformation
                    when (fullVerbosity opts) (putStrLn "Creating Output...")
                    return $ generateOutput requests' results

-- This action extracts or generates the requested information for the input, depending on whether the information
-- already exists or not.
extractOrGenerate :: Options -> Configuration a b -> a -> [b] -> String -> IO (Maybe b)
extractOrGenerate opts conf input infos request = do
    when (fullVerbosity opts) (putStrLn $ "Looking up extractor and generator for request '" ++ request ++ "'...")
    case lookup request conf of
        Nothing                     -> do
            when (fullVerbosity opts) (putStrLn $ "Entry '" ++ request ++ "' not found in configuration.")
            return Nothing
        Just (extractor, generator) -> do
            when (fullVerbosity opts) (putStrLn $ "Extractor and Generator for request '" ++ request ++ "' found. Looking at force option...")
            case optForce opts of
                2 -> do
                    when (fullVerbosity opts) (putStrLn $ "Force option is 2. Generating information for request '" ++ request ++ "'...")
                    return $ generator opts input
                1 -> do
                    when (fullVerbosity opts) (putStrLn $ "Force option is 1. Extracting/Generating information for request '" ++ request ++ "'...")
                    maybe (generator opts input) (return . Just) (extractor infos)
                0 -> do
                    when (fullVerbosity opts) (putStrLn $ "Force option is 0. Only Extracting information for request '" ++ request ++ "'...")
                    return $ extractor infos

-- This function generates an output for the fields and respective results of extracting or generating.
generateOutput :: JPretty a => [String] -> [Maybe a] -> Output
generateOutput fields results =
    let outputs = zipWith (\f r -> maybe (f, "failed") jsonOutput r) fields results
    in OutputText $ unlines $ map (\(f, m) -> f ++ ": " ++ m) outputs

-- This operator combines two lists and excludes all dublicates. The first list should contain the newer information
-- to get an updated list.
(<+>) :: EqInfo a => [a] -> [a] -> [a]
info1 <+> info2 = nubBy sameInfo (info1 ++ info2)

main :: IO ()
main = do
        args <- getArgs
        (opts, args2) <- processOptions "" args

        let pkg = extractOpt "packages"     (optPackage opts)
        unless (isJust pkg) (putStrLn "Package name is required" >> exitWith 1)

        let vsn = extractOpt "versions"     (optVersion opts)
        let m   = extractOpt "modules"      (optModule opts)
        let t   = extractOpt "types"        (optType opts)
        let c   = extractOpt "typeclasses"  (optTypeclass opts)
        let op  = extractOpt "operations"   (optOperation opts)

        let obj = catMaybes [pkg, vsn, m, t, c, op]

        res <- getInfos opts obj args2
        print res
    where
        extractOpt :: String -> Maybe String -> Maybe (String, String)
        extractOpt tag = fmap (\x -> (tag, x))
