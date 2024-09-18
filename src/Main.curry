module Main where

import CurryInfo.Configuration
import CurryInfo.Paths (Path, initialize, index, getJSONPath)
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.Options (getObject, processOptions, queryOptions)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Generator (readPackageJSON, getExportedModules, readPackageModules)

import JSON.Pretty (ppJSON)
import JSON.Data
import JSON.Parser (parseJSON)

import Data.Maybe (isJust)
import Data.Either (partitionEithers)

import System.Environment (getArgs)
import System.Process (exitWith)
import System.FilePath ((</>), (<.>))
import System.Directory (doesDirectoryExist, doesFileExist)

import Control.Monad (unless, zipWithM, when)

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
            let obj = CurryPackage pkg
            result <- checkPackageExists obj
            case result of
                False -> do
                    let err = "Package '" ++ pkg ++ "' does not exist."
                    printDebugMessage opts err
                    return $ OutputError err
                True -> do
                    printDebugMessage opts "Package exists."
                    getInfos' packageConfiguration obj
        [("packages", pkg), ("versions", vsn)]                                      -> do
            printStatusMessage opts "Structure matches Version."
            let obj = CurryVersion pkg vsn
            result <- checkVersionExists obj
            case result of
                False -> do
                    let err = "Version '" ++ vsn ++ "' of package '" ++ pkg ++ "' does not exists."
                    printDetailMessage opts err
                    return $ OutputError $ err
                True -> do
                    printDetailMessage opts "Checked Version: exists."
                    getInfos' versionConfiguration obj
        [("packages", pkg), ("versions", vsn), ("modules", m)]                      -> do
            printStatusMessage opts "Structure matches Module."
            let obj = CurryModule pkg vsn m
            result <- checkModuleExists obj
            case result of
                False -> do
                    let err = "Module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checked Module: exists."
                    getInfos' moduleConfiguration obj
        [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)]        -> do
            printStatusMessage opts "Structure matches Type"
            let obj = CurryType pkg vsn m t
            result <- checkTypeExists obj
            case result of
                False -> do
                    let err = "Type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checked Type: exists."
                    getInfos' typeConfiguration obj
        [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)]  -> do
            printStatusMessage opts "Structure matches Typeclass"
            let obj = CurryTypeclass pkg vsn m c
            result <- checkTypeclassExists obj
            case result of
                False -> do
                    let err = "Typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checked Typeclass: exists."
                    getInfos' typeclassConfiguration obj
        [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)]   -> do
            printStatusMessage opts "Structure matches Operation."
            let obj = CurryOperation pkg vsn m o
            result <- checkOperationExists obj
            case result of
                False -> do
                    let err = "Operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    return $ OutputError err
                True  -> do
                    printDetailMessage opts "Checker Operation: exists."
                    getInfos' operationConfiguration obj
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
                    return $ OutputError $ errorReading obj
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

                            let (failedRequests, successfulRequests) = partitionEithers results
                            let errs = unlines $ map (\(r, m) -> r ++ ": " ++ m) failedRequests
                            when (not (null failedRequests)) (printStatusMessage opts errs)
                            let newInformation = map (\(r, jv, _) -> (r, jv)) successfulRequests <+> fields
                            printDebugMessage opts "Overwriting with updated information..."
                            writeInformation obj newInformation
                            printDetailMessage opts "Overwriting finished."
                            printDebugMessage opts "Creating output..."
                            let outs = map (either id (\(r, _, s) -> (r, s))) results
                            output <- generateOutput outs
                            printDetailMessage opts "Output created."
                            return output
        
        extractOrGenerate :: ErrorMessage a => [RegisteredRequest a] -> [(String, JValue)] -> a -> String -> IO (Either (String, String) (String, JValue, String))-- (String, Either String (JValue, String))
        extractOrGenerate conf fields obj req = do
            printStatusMessage opts $ "\nProcessing request '" ++ req ++ "'..."
            case lookupRequest req conf of
                Nothing -> do
                    let msg = errorRequest obj req
                    printDetailMessage opts $ msg
                    return $ Left (req, msg)
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
                        v -> do
                            let msg = "INVALID FORCE OPTION: " ++ show v
                            printDebugMessage opts msg
                            return (Left (req, msg))

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
        
        checkPackageExists :: CurryPackage -> IO Bool
        checkPackageExists obj@(CurryPackage pkg) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
                    path <- index
                    printDebugMessage opts $ "Looking for package in index..."
                    b2 <- doesDirectoryExist (path </> pkg)
                    return b2

        checkVersionExists :: CurryVersion -> IO Bool
        checkVersionExists obj@(CurryVersion pkg vsn) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
                    path <- index
                    printDebugMessage opts $ "Looking for version in index..."
                    b2 <- doesDirectoryExist (path </> pkg </> vsn)
                    return b2

        checkModuleExists :: CurryModule -> IO Bool
        checkModuleExists obj@(CurryModule pkg vsn m) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
                    allMods <- readPackageModules opts pkg vsn

                    packageJSON <- readPackageJSON opts pkg vsn
                    let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

                    return (elem m exportedMods)

        checkTypeExists :: CurryType -> IO Bool
        checkTypeExists obj@(CurryType pkg vsn m t) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
                    res <- getInfos queryOptions [("packages", pkg), ("versions", vsn), ("modules", m)] ["types"]
                    case res of
                        OutputTerm terms -> case lookup "types" terms of
                            Nothing -> return False
                            Just ts -> do
                                return $ elem t (read ts)
                        _ -> return False

        checkTypeclassExists :: CurryTypeclass -> IO Bool
        checkTypeclassExists obj@(CurryTypeclass pkg vsn m c) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
                    res <- getInfos queryOptions [("packages", pkg), ("versions", vsn), ("modules", m)] ["typeclasses"]
                    case res of
                        OutputTerm terms -> case lookup "typeclasses" terms of
                            Nothing -> return False
                            Just cs -> do
                                return $ elem c (read cs)
                        _ -> return False

        checkOperationExists :: CurryOperation -> IO Bool
        checkOperationExists obj@(CurryOperation pkg vsn m o) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
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
