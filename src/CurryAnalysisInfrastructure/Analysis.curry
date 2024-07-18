module CurryAnalysisInfrastructure.Analysis where

import CurryAnalysisInfrastructure.Commands
import CurryAnalysisInfrastructure.Options
import CurryAnalysisInfrastructure.Types

import JSON.Data
import JSON.Parser (parseJSON)

import Data.List (init, find)

import System.IO.Unsafe (trace)

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

analyseSafeModule :: Options -> String -> Module -> IO (Maybe Bool)
analyseSafeModule opts path m = do
    (exitCode, output, err) <- runCmd opts (cmdCASS path "UnsafeModule" m)

    case parseJSON (init output) of
        Just (JArray js) -> do
            case findField js m of
                Just result -> case result of
                    "safe" -> return $ Just True
                    "unsafe" -> return $ Just False
                    _ -> do
                        print "JValue does not match expected form"
                        return Nothing
                Nothing -> do
                    print "JValue does not match expected form"
                    return Nothing
        Just _ -> do
            print "JValue does not match expected form"
            return Nothing
        Nothing -> do
            print "Parsing of JValue failed"
            return Nothing

analyseDeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Determinism)
analyseDeterministic opts path m op = do
    (exitCode, output, err) <- runCmd opts (cmdCASS path "Deterministic" m)

    case parseJSON (init output) of
        Just (JArray js) -> do
            case findField js op of
                Just result -> case result of
                    "deterministic" -> return $ Just Det
                    "non-deterministic" -> return $ Just NDet
                    _ -> do
                        print "JValue does not match expected form"
                        return Nothing
                Nothing -> do
                    print "JValue does not match expected form"
                    return Nothing
        Just _ -> do
            print "JValue does not match expected form"
            return Nothing
        Nothing -> do
            print "Parsing of JValue failed"
            return Nothing

testPath :: String
testPath = "/home/dennis/tmp/.curryanalysis/checkouts/json-3.0.0"

testModule :: Module
testModule = "JSON.Data"

testOptions :: Options
testOptions = defaultOptions