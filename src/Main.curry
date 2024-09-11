module Main where

import CurryInfo.Configuration
import CurryInfo.Paths (Path, initialize)
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.ErrorMessage
import CurryInfo.Options (getObject, processOptions)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)

import JSON.Pretty (ppJSON)
import JSON.Data

import Data.Maybe (isJust, fromJust)
import Data.List (nubBy)

import System.Environment (getArgs)
import System.Process (exitWith)

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
                    printDebugMessage opts "Package does not exist."
                    return $ OutputError $ "Package '" ++ pkg ++ "' does not exist."
                True -> do
                    printDebugMessage opts "Package exists."
                    getInfos' packageConfiguration (CurryPackage pkg)
        [("packages", pkg), ("versions", vsn)]                                      -> do
            printStatusMessage opts "Structure matches Version."
            result <- checkVersionExists pkg vsn
            case result of
                False -> do
                    printDetailMessage opts "Checked Version: does not exist."
                    return $ OutputError $ "Version '" ++ vsn ++ "' of package '" ++ pkg ++ "' does not exist."
                True -> do
                    printDetailMessage opts "Checked Version: exists."
                    getInfos' versionConfiguration (CurryVersion pkg vsn)
        [("packages", pkg), ("versions", vsn), ("modules", m)]                      -> do
            printStatusMessage opts "Structure matches Module."
            result <- checkModuleExists pkg vsn m
            case result of
                False -> do
                    printDetailMessage opts "Checked Module: does not exist or is not exported."
                    return $ OutputError $ "Module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                True  -> do
                    printDetailMessage opts "Checked Module: exists."
                    getInfos' moduleConfiguration (CurryModule pkg vsn m)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)]        -> do
            printStatusMessage opts "Structure matches Type"
            result <- checkTypeExists pkg vsn m t
            case result of
                False -> do
                    printDetailMessage opts "Checked Type: does not exist or is not exported."
                    return $ OutputError $ "Type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                True  -> do
                    printDetailMessage opts "Checked Type: exists."
                    getInfos' typeConfiguration (CurryType pkg vsn m t)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)]  -> do
            printStatusMessage opts "Structure matches Typeclass"
            result <- checkTypeclassExists pkg vsn m c
            case result of
                False -> do
                    printDetailMessage opts "Checked Typeclass: does not exist or is not exported."
                    return $ OutputError $ "Typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                True  -> do
                    printDetailMessage opts "Checked Typeclass: exists."
                    getInfos' typeclassConfiguration (CurryTypeclass pkg vsn m c)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)]   -> do
            printStatusMessage opts "Structure matches Operation."
            result <- checkOperationExists pkg vsn m o
            case result of
                False -> do
                    printDetailMessage opts "Checker Operation: does not exist or is not exported."
                    return $ OutputError $ "Operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
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
                            printDebugMessage opts "No requests. Returning all currently available information..."
                            let fieldNames = map fst fields
                            case mapM (flip lookupRequest conf) fieldNames of
                                Nothing -> do
                                    return $ OutputError $ "One of the fields could not be found: " ++ show fieldNames
                                Just allReqs -> do
                                    results <- zipWithM (\(_, _, extractor, _) fieldName -> fmap (\x -> (fieldName, x)) (extract extractor fields)) allReqs fieldNames
                                    let mouts = map (\(req, mres) -> (req, fmap snd mres)) results
                                    printDebugMessage opts "Creating output..."
                                    output <- generateOutput conf mouts
                                    printDetailMessage opts "Output created."
                                    return output
                        False -> do
                            printDetailMessage opts "Extracting/Generating requested information..."
                            results <- mapM (extractOrGenerate conf fields obj) reqs

                            let successfulFields = map (\(req, mres) -> (req, fst (fromJust mres))) $ filter (\(_, mres) -> isJust mres) results
                            let mouts = map (\(req, mres) -> (req, fmap snd mres)) results

                            let newInformation = successfulFields <+> fields
                            printDebugMessage opts "Overwriting with updated information..."
                            writeInformation obj newInformation
                            printDetailMessage opts "Overwriting finished."
                            printDebugMessage opts "Creating output..."
                            output <- generateOutput conf mouts
                            printDetailMessage opts "Output created."
                            return output
        
        extractOrGenerate :: [RegisteredRequest a] -> [(String, JValue)] -> a -> String -> IO (String, Maybe (JValue, String))
        extractOrGenerate conf fields obj req = do
            printStatusMessage opts $ "\nProcessing request '" ++ req ++ "'..."
            case lookupRequest req conf of
                Nothing -> do
                    printDetailMessage opts $ "Could not find request '" ++ req ++ "'."
                    return (req, Nothing)
                Just (_, _, extractor, generator) -> do
                    printDetailMessage opts "Found request. Looking at Force option..."
                    case optForce opts of
                        0 -> do
                            printDebugMessage opts "Force option 0: Only extraction"
                            extractionResult <- extract extractor fields
                            case extractionResult of
                                Nothing -> do
                                    printDetailMessage opts "Executing request failed"
                                    return (req, Nothing)
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return $ (req, Just (jv, output))
                        1 -> do
                            printDebugMessage opts "Force option 1: Extraction, then generation if failed"
                            extractionResult <- extract extractor fields
                            case extractionResult of
                                Nothing -> do
                                    generationResult <- generate generator obj
                                    case generationResult of
                                        Nothing -> do
                                            printDetailMessage opts "Executing request failed."
                                            return (req, Nothing)
                                        Just (jv, output) -> do
                                            printDetailMessage opts "Executing request succeeded."
                                            return $ (req, Just (jv, output))
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return $ (req, Just (jv, output))
                        2 -> do
                            printDebugMessage opts "Force option 2: Only generation"
                            generationResult <- generate generator obj
                            case generationResult of
                                Nothing -> do
                                    printDetailMessage opts "Executing request failed"
                                    return (req, Nothing)
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return $ (req, Just (jv, output))

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
        
        generateOutput :: [RegisteredRequest a] -> [(String, Maybe String)] -> IO Output
        generateOutput conf mouts = do
            --outputs :: (String, Maybe String)
            case optOutput opts of
                OutText -> do
                    let outs = map (\(req, mout) -> req ++ ": " ++ maybe "FAILED" id mout) mouts
                    return $ OutputText (unlines outs)
                OutJSON -> do
                    let outs = map (\(req, mout) -> (req, JString (maybe "FAILED" id mout))) mouts
                    return $ OutputJSON (JObject outs)
                OutTerm -> do
                    let outs = map (\(req, mout) -> (req, maybe "FAILED" id mout)) mouts
                    return $ OutputTerm outs
        
        checkPackageExists :: Package -> IO Bool
        checkPackageExists pkg = do
            putStrLn "NOT YET IMPLEMENTED: checkPackageExists"
            return True

        checkVersionExists :: Package -> Version -> IO Bool
        checkVersionExists pkg vsn = do
            putStrLn "NOT YET IMPLEMENTED: checkVersionExists"
            return True

        checkModuleExists :: Package -> Version -> Module -> IO Bool
        checkModuleExists pkg vsn m = do
            putStrLn "NOT YET IMPLEMENTED: checkModuleExists"
            return True

        checkTypeExists :: Package -> Version -> Module -> Type -> IO Bool
        checkTypeExists pkg vsn m t = do
            putStrLn "NOT YET IMPLEMENTED: checkTypeExists"
            return True

        checkTypeclassExists :: Package -> Version -> Module -> Typeclass -> IO Bool
        checkTypeclassExists pkg vsn m c = do
            putStrLn "NOT YET IMPLEMENTED: checkTypeclassExists"
            return True

        checkOperationExists :: Package -> Version -> Module -> Operation -> IO Bool
        checkOperationExists pkg vsn m o = do
            putStrLn "NOT YET IMPLEMENTED: checkOperationExists"
            return True

main :: IO ()
main = do
        args <- getArgs
        (opts, args2) <- processOptions "" args
        let obj = getObject opts
        unless (isJust (optPackage opts)) (putStrLn "Package name is required" >> exitWith 1)
        res <- getInfos opts obj args2
        printResult res
