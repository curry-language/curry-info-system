module Main where

import CurryInfo.Configuration
import CurryInfo.Paths (Path, initialize, index)
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.ErrorMessage
import CurryInfo.Options (getObject, processOptions, queryOptions)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Generator (readPackageJSON, getExportedModules, readPackageModules)

import JSON.Pretty (ppJSON)
import JSON.Data
import JSON.Parser (parseJSON)

import Data.Maybe (isJust, fromJust)
import Data.List (nubBy)
import Data.Either (partitionEithers)

import System.Environment (getArgs)
import System.Process (exitWith)
import System.FilePath ((</>), (<.>))
import System.Directory (doesDirectoryExist)

import Control.Monad (unless, zipWithM)

-- This operator combines two lists and excludes all dublicates. The first list should contain the newer information
-- to get an updated list.
(<+>) :: [(String, a)] -> [(String, a)] -> [(String, a)]
info1 <+> info2 = nubBy (\(k1, _) (k2, _) -> k1 == k2) (info1 ++ info2)

printResult :: Output -> IO ()
printResult (OutputText txt) = putStrLn txt
printResult (OutputJSON jv) = putStrLn $ ppJSON jv
printResult (OutputTerm ts) = putStrLn $ show ts
printResult (OutputError err) = putStrLn $ "Error: " ++ err

getInfos :: Options -> [(String, String)] -> [String] -> IO Output
getInfos opts input reqs = do
    printStatusMessage opts "Checking structure of the request..."
    case input of
        [("packages", pkg)] -> do
            printStatusMessage opts "Structure matches Package."
            result <- checkPackageExists pkg
            case result of
                False -> do
                    let err = "Package '" ++ pkg ++ "' does not exist."
                    printDebugMessage opts err
                    return $ OutputError err
                True -> do
                    printDebugMessage opts "Package exists."
                    getInfos' packageConfiguration (CurryPackage pkg)
        [("packages", pkg), ("versions", vsn)]                                      -> do
            printStatusMessage opts "Structure matches Version."
            result <- checkVersionExists pkg vsn
            case result of
                False -> do
                    let err = "Version '" ++ vsn ++ "' of package '" ++ pkg ++ "' does not exists."
                    printDetailMessage opts err
                    return $ OutputError $ err
                True -> do
                    printDetailMessage opts "Checked Version: exists."
                    getInfos' versionConfiguration (CurryVersion pkg vsn)
        [("packages", pkg), ("versions", vsn), ("modules", m)]                      -> do
            printStatusMessage opts "Structure matches Module."
            result <- checkModuleExists pkg vsn m
            case result of
                False -> do
                    let err = "Module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checked Module: exists."
                    getInfos' moduleConfiguration (CurryModule pkg vsn m)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)]        -> do
            printStatusMessage opts "Structure matches Type"
            result <- checkTypeExists pkg vsn m t
            case result of
                False -> do
                    let err = "Type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checked Type: exists."
                    getInfos' typeConfiguration (CurryType pkg vsn m t)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)]  -> do
            printStatusMessage opts "Structure matches Typeclass"
            result <- checkTypeclassExists pkg vsn m c
            case result of
                False -> do
                    let err = "Typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checked Typeclass: exists."
                    getInfos' typeclassConfiguration (CurryTypeclass pkg vsn m c)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)]   -> do
            printStatusMessage opts "Structure matches Operation."
            result <- checkOperationExists pkg vsn m o
            case result of
                False -> do
                    let err = "Operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checker Operation: exists."
                    getInfos' operationConfiguration (CurryOperation pkg vsn m o)
        _ -> return $ OutputError $ show input ++ " does not match any pattern"
    where
        getInfos' conf obj = do
            printStatusMessage opts "Initializing Input..."
            initialize obj
            printStatusMessage opts "Reading current information..."
            mfields <- readInformation opts obj
            case mfields of
                Nothing -> do
                    printDetailMessage opts "Reading information failed."
                    return $ OutputError $ errorMessage obj
                Just fields -> do
                    printDetailMessage opts "Reading information succeeded."
                    case optShowAll opts of
                        True -> do
                            printStatusMessage opts "Returning all currently available information..."
                            let fieldNames = map fst fields
                            case mapM (flip lookupRequest conf) fieldNames of
                                Nothing -> do
                                    return $ OutputError $ "One of the fields could not be found: " ++ show fieldNames
                                Just allReqs -> do
                                    results <- zipWithM (\(_, _, extractor, _) fieldName -> fmap (\x -> (fieldName, x)) (extract extractor fields)) allReqs fieldNames
                                    let mouts = map (\(req, mres) -> (req, fmap snd mres)) results
                                    printDebugMessage opts "Creating output..."
                                    output <- generateOutput (map (\(req, mout) -> (req, maybe "Extracting information failed" id mout)) mouts)
                                    printDetailMessage opts "Output created."
                                    return output
                        False -> do
                            printDetailMessage opts "Extracting/Generating requested information..."
                            results <- mapM (extractOrGenerate conf fields obj) reqs

                            let (_, successfulRequests) = partitionEithers results
                            let newInformation = map (\(r, jv, _) -> (r, jv)) successfulRequests <+> fields
                            printDebugMessage opts "Overwriting with updated information..."
                            writeInformation obj newInformation
                            printDetailMessage opts "Overwriting finished."
                            printDebugMessage opts "Creating output..."
                            let outs = map (either id (\(r, _, s) -> (r, s))) results
                            output <- generateOutput outs
                            printDetailMessage opts "Output created."
                            return output
        
        extractOrGenerate :: [RegisteredRequest a] -> [(String, JValue)] -> a -> String -> IO (Either (String, String) (String, JValue, String))-- (String, Either String (JValue, String))
        extractOrGenerate conf fields obj req = do
            printStatusMessage opts $ "\nProcessing request '" ++ req ++ "'..."
            case lookupRequest req conf of
                Nothing -> do
                    printDetailMessage opts $ "Could not find request '" ++ req ++ "'."
                    return $ Left (req, "Could not find request.")
                Just (_, _, extractor, generator) -> do
                    printDetailMessage opts "Found request. Looking at Force option..."
                    case optForce opts of
                        0 -> do
                            printDebugMessage opts "Force option 0: Only extraction"
                            extractionResult <- extract extractor fields
                            case extractionResult of
                                Nothing -> do
                                    printDetailMessage opts "Executing request failed"
                                    return $ Left (req, "Extracting information failed.")
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return $ Right (req, jv, output)
                        1 -> do
                            printDebugMessage opts "Force option 1: Extraction, then generation if failed"
                            extractionResult <- extract extractor fields
                            case extractionResult of
                                Nothing -> do
                                    generationResult <- generate generator obj
                                    case generationResult of
                                        Nothing -> do
                                            printDetailMessage opts "Executing request failed."
                                            return $ Left (req, "Extracting and Generating information failed.")
                                        Just (jv, output) -> do
                                            printDetailMessage opts "Executing request succeeded."
                                            return $ Right (req, jv, output)
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return $ Right (req, jv, output)
                        2 -> do
                            printDebugMessage opts "Force option 2: Only generation"
                            generationResult <- generate generator obj
                            case generationResult of
                                Nothing -> do
                                    printDetailMessage opts "Executing request failed"
                                    return $ Left (req, "Generating information failed.")
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return $ Right (req, jv, output)

        extract :: (Options -> [(String, JValue)] -> IO (Maybe (JValue, String))) -> [(String, JValue)] -> IO (Maybe (JValue, String))
        extract extractor fields = do
            printDebugMessage opts "Trying extraction..."
            extractionResult <- extractor opts fields
            case extractionResult of
                Nothing -> do
                    printDetailMessage opts "Extraction failed."
                    return Nothing
                Just (jv, output) -> do
                    printDetailMessage opts "Extraction succeeded."
                    return $ Just (jv, output)
        
        generate :: (Options -> a -> IO (Maybe (JValue, String))) -> a -> IO (Maybe (JValue, String))
        generate generator obj = do
            printDebugMessage opts "Trying generation..."
            generationResult <- generator opts obj
            case generationResult of
                Nothing -> do
                    printDetailMessage opts "Generation failed."
                    return Nothing
                Just (jv, output) -> do
                    printDetailMessage opts "Generation succeeded."
                    return $ Just (jv, output)
        
        generateOutput :: [(String, String)] -> IO Output
        generateOutput outs = do
            case optOutput opts of
                OutText -> do
                    let msgs = map (\(req, out) -> req ++ ": " ++ out) outs
                    return $ OutputText (unlines msgs)
                OutJSON -> do
                    let fields = map (\(req, out) -> (req, JString out)) outs
                    return $ OutputJSON (JObject fields)
                OutTerm -> do
                    return $ OutputTerm outs
        
        checkPackageExists :: Package -> IO Bool
        checkPackageExists pkg = do
            path <- index
            printDebugMessage opts $ "Looking for package in index..."
            b <- doesDirectoryExist (path </> pkg)
            return b

        checkVersionExists :: Package -> Version -> IO Bool
        checkVersionExists pkg vsn = do
            path <- index
            printDebugMessage opts $ "Looking for version in index..."
            b <- doesDirectoryExist (path </> pkg </> vsn)
            return b

        checkModuleExists :: Package -> Version -> Module -> IO Bool
        checkModuleExists pkg vsn m = do
            allMods <- readPackageModules opts pkg vsn

            packageJSON <- readPackageJSON opts pkg vsn
            let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

            return (elem m exportedMods)

        checkTypeExists :: Package -> Version -> Module -> Type -> IO Bool
        checkTypeExists pkg vsn m t = do
            res <- getInfos queryOptions [("packages", pkg), ("versions", vsn), ("modules", m)] ["types"]
            case res of
                OutputTerm terms -> case lookup "types" terms of
                    Nothing -> return False
                    Just ts -> do
                        return $ elem t (read ts)
                _ -> return False

        checkTypeclassExists :: Package -> Version -> Module -> Typeclass -> IO Bool
        checkTypeclassExists pkg vsn m c = do
            res <- getInfos queryOptions [("packages", pkg), ("versions", vsn), ("modules", m)] ["typeclasses"]
            case res of
                OutputTerm terms -> case lookup "typeclasses" terms of
                    Nothing -> return False
                    Just cs -> do
                        return $ elem c (read cs)
                _ -> return False

        checkOperationExists :: Package -> Version -> Module -> Operation -> IO Bool
        checkOperationExists pkg vsn m o = do
            res <- getInfos queryOptions [("packages", pkg), ("versions", vsn), ("modules", m)] ["operations"]
            case res of
                OutputTerm terms -> case lookup "operations" terms of
                    Nothing -> return False
                    Just os -> do
                        return $ elem o (read os)
                _ -> return False

main :: IO ()
main = do
        args <- getArgs
        (opts, args2) <- processOptions "" args
        let obj = getObject opts
        unless (isJust (optPackage opts)) (putStrLn "Package name is required" >> exitWith 1)
        res <- getInfos opts obj args2
        printResult res
