module CurryAnalysisInfrastructure.Analysis where

import CurryAnalysisInfrastructure.Commands
import CurryAnalysisInfrastructure.Options
import CurryAnalysisInfrastructure.Types

import JSON.Data
import JSON.Parser (parseJSON)

import Data.List (init, find)

import Control.Monad (when)

data Determinism
    = Det
    | NDet
    deriving (Show, Read)

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

analyse :: Options -> String -> String -> Module -> String -> [(String, a)] -> IO (Maybe a)
analyse opts path analysis m field x = do
    when (fullVerbosity opts) (putStrLn $ "Starting analysis " ++ analysis ++ "...")
    (exitCode, output, err) <- runCmd opts (cmdCASS path analysis m)
    when (fullVerbosity opts) (putStrLn $ "Analysis finished.")
    when (fullVerbosity opts) (putStrLn $ "Parsing result...")
    case parseJSON (init output) of 
        Just (JArray js) -> do
            case findField js field of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis succeeded.")
                    return $ lookup result x
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

analyseSafeModule :: Options -> String -> Module -> IO (Maybe Bool)
analyseSafeModule opts path m = do
    analyse opts path "UnsafeModule" m m [("safe", True), ("unsafe", False)]

analyseDeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Determinism)
analyseDeterministic opts path m op = do
    analyse opts path "Deterministic" m op [("deterministic", Det), ("non-deterministic", NDet)]

testPath :: String
testPath = "/home/dennis/tmp/.curryanalysis/checkouts/json-3.0.0"

testModule :: Module
testModule = "JSON.Parser"

testOp :: Operation
testOp = "parseJSON"

testOptions :: Options
testOptions = defaultOptions