module CurryAnalysisInfrastructure.Analysis where

import CurryAnalysisInfrastructure.Commands
import CurryAnalysisInfrastructure.Options
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Parser
    ( parseSafe, parseDeterministic, parseDemandness, parseIndeterministic, parseSolutionCompleteness, parseTermination
    , parseTotallyDefined
    )

import JSON.Data
import JSON.Parser (parseJSON)

import Data.List (init, find, intercalate)

import Control.Monad (when)

-- Analysis

-- This operation looksup a field with a specific name in a javascript object. If not such field can be found or
-- the javascript value is not a javascript object, then it will return Nothing.
findField :: [JValue] -> String -> Maybe String
findField js field = do
        found <- find (checker field) js
        case found of
            JObject [_, _, ("result", JString result)] -> return result
            _ -> Nothing
    where
        checker :: String -> JValue -> Bool
        checker f1 v = case v of
            JObject [_, ("name", JString f2), _] -> f1 == f2
            _ -> False

-- This action initiates a call to CASS to compute the given analysis for the given module.
-- The parser argument is for parsing the result of the analysis.
analyse :: Options -> String -> String -> Module -> String -> (String -> Maybe a) -> IO (Maybe a)
analyse opts path analysis m field parser = do
    when (fullVerbosity opts) (putStrLn $ "Starting analysis " ++ analysis ++ "...")
    (_, output, _) <- runCmd opts (cmdCASS path analysis m)
    when (fullVerbosity opts) (putStrLn $ "Analysis finished.")
    when (fullVerbosity opts) (putStrLn $ "Parsing result...")
    case parseJSON (init output) of 
        Just (JArray js) -> do
            case findField js field of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis succeeded.")
                    return $ parser result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Could not find entry with name '" ++ field ++ "'.")
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    return Nothing
        _ -> do
            when (fullVerbosity opts) (putStrLn $ "Output did not match expected format. Expected array.")
            when (fullVerbosity opts) (putStrLn $ "Output:")
            when (fullVerbosity opts) (putStrLn $ output)
            when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
            return Nothing

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis for the given module in
-- the given path.
analyseSafeModule :: Options -> String -> Module -> IO (Maybe Safe)
analyseSafeModule opts path m = do
    analyse opts path "UnsafeModule" m m parseSafe

-- This action initiates a call to CASS to compute the 'Deterministic' analysis for the given module in
-- the given path.
analyseDeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Deterministic)
analyseDeterministic opts path m op = do
    analyse opts path "Deterministic" m op parseDeterministic

-- This action initiates a call to CASS to compute the 'Demand' analysis for the given module in
-- the given path.
analyseDemandness :: Options -> String -> Module -> Operation -> IO (Maybe Demandness)
analyseDemandness opts path m op = do
    analyse opts path "Demand" m op parseDemandness

-- This action initiates a call to CASS to compute the 'Indeterministic' analysis for the given module in
-- the given path.
analyseIndeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Indeterministic)
analyseIndeterministic opts path m op = do
    analyse opts path "Indeterministic" m op parseIndeterministic

-- This action initiates a call to CASS to compute the 'SolComplete' analysis for the given module in
-- the given path.
analyseSolutionCompleteness :: Options -> String -> Module -> Operation -> IO (Maybe SolutionCompleteness)
analyseSolutionCompleteness opts path m op = do
    analyse opts path "SolComplete" m op parseSolutionCompleteness

-- This action initiates a call to CASS to compute the 'Terminating' analysis for the given module in
-- the given path.
analyseTermination :: Options -> String -> Module -> Operation -> IO (Maybe Termination)
analyseTermination opts path m op = do
    analyse opts path "Terminating" m op parseTermination

-- This action initiates a call to CASS to compute the 'Total' analysis for the given module in
-- the given path.
analyseTotallyDefined :: Options -> String -> Module -> Operation -> IO (Maybe TotallyDefined)
analyseTotallyDefined opts path m op = do
    analyse opts path "Total" m op parseTotallyDefined

{-
testPath :: String
testPath = "/home/dennis/tmp/.curryanalysis/checkouts/json-3.0.0"

testModule :: Module
testModule = "JSON.Parser"

testOp :: Operation
testOp = "parseJSON"

testOptions :: Options
testOptions = defaultOptions {optVerb = 4}
-}