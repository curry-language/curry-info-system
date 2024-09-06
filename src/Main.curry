module Main where

import CurryInfo.Configuration
import CurryInfo.Paths (Path, initialize)
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.ErrorMessage
import CurryInfo.Options (getObject, processOptions)
import CurryInfo.Verbosity (printLine, printDebugMessage)

import JSON.Pretty (ppJSON)
import JSON.Data

import Data.Maybe (isJust, fromJust)
import Data.List (nubBy)

import System.Environment (getArgs)
import System.Process (exitWith)

import Control.Monad (unless)

-- This operator combines two lists and excludes all dublicates. The first list should contain the newer information
-- to get an updated list.
(<+>) :: [(String, a)] -> [(String, a)] -> [(String, a)]
info1 <+> info2 = nubBy (\(k1, _) (k2, _) -> k1 == k2) (info1 ++ info2)

printResult :: Output -> IO ()
printResult (OutputText txt) = do
    putStrLn ""
    putStrLn "Finished with OutputText"
    putStrLn txt
printResult (OutputError err) = do
    putStrLn ""
    putStrLn "Finished with OutputError"
    putStrLn err
printResult (OutputJSON jv) = do
    putStrLn ""
    putStrLn "Finished with OutputJSON"
    putStrLn $ ppJSON jv

getInfos2 :: Options -> [(String, String)] -> [String] -> IO Output
getInfos2 opts input reqs = do
    printLine opts
    printDebugMessage opts "Checking structure of the request..."
    case input of
        [("packages", pkg)] -> do
            printDebugMessage opts "Structure matches Package."
            result <- checkPackageExists pkg
            case result of
                False -> do
                    printDebugMessage opts "Package does not exist."
                    return $ OutputError $ "Package '" ++ pkg ++ "' does not exist."
                True -> do
                    printDebugMessage opts "Package exists."
                    getInfos' packageConfiguration (CurryPackage pkg)
        [("packages", pkg), ("versions", vsn)]                                      -> do
            printDebugMessage opts "Structure matches Version."
            result <- checkVersionExists pkg vsn
            case result of
                False -> do
                    printDebugMessage opts "Version does not exist."
                    return $ OutputError $ "Version '" ++ vsn ++ "' of package '" ++ pkg ++ "' does not exist."
                True -> do
                    printDebugMessage opts "Version exists."
                    getInfos' versionConfiguration (CurryVersion pkg vsn)
        [("packages", pkg), ("versions", vsn), ("modules", m)]                      -> do
            printDebugMessage opts "Structure matches Module."
            result <- checkModuleExists pkg vsn m
            case result of
                False -> do
                    printDebugMessage opts "Module does not exist or is not exported."
                    return $ OutputError $ "Module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                True  -> do
                    printDebugMessage opts "Module exists."
                    getInfos' moduleConfiguration (CurryModule pkg vsn m)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)]        -> do
            printDebugMessage opts "Structure matches Type"
            result <- checkTypeExists pkg vsn m t
            case result of
                False -> do
                    printDebugMessage opts "Type does not exist or is not exported."
                    return $ OutputError $ "Type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                True  -> do
                    printDebugMessage opts "Type exists."
                    getInfos' typeConfiguration (CurryType pkg vsn m t)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)]  -> do
            printDebugMessage opts "Structure matches Typeclass"
            result <- checkTypeclassExists pkg vsn m c
            case result of
                False -> do
                    printDebugMessage opts "Typeclass does not exist or is not exported."
                    return $ OutputError $ "Typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                True  -> do
                    printDebugMessage opts "Typeclass exists."
                    getInfos' typeclassConfiguration (CurryTypeclass pkg vsn m c)
        [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)]   -> do
            printDebugMessage opts "Structure matches Operation."
            result <- checkOperationExists pkg vsn m o
            case result of
                False -> do
                    printDebugMessage opts "Operation does not exist or is not exported."
                    return $ OutputError $ "Operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                True  -> do
                    printDebugMessage opts "Operation exists."
                    getInfos' operationConfiguration (CurryOperation pkg vsn m o)
        _ -> return $ OutputError $ show input ++ " does not match any pattern"
    where
        getInfos' conf obj = do
            printLine opts
            printDebugMessage opts "Initializing Input..."
            initialize obj
            printDebugMessage opts "Reading current information..."
            mfields <- readInformation opts obj
            case mfields of
                Nothing -> do
                    printDebugMessage opts "Reading information failed."
                    return $ OutputError $ errorMessage obj
                Just fields -> do
                    printDebugMessage opts "Reading information succeeded."
                    printDebugMessage opts "Extracting/Generating requested information..."
                    results <- mapM (extractOrGenerate' conf fields obj) reqs

                    let successfulFields = map (\(req, mres) -> (req, fst (fromJust mres))) $ filter (\(_, mres) -> isJust mres) results
                    let mouts = map (\(req, mres) -> (req, fmap snd mres)) results

                    let newInformation = successfulFields <+> fields
                    printDebugMessage opts "Overwriting with updated information..."
                    writeInformation obj newInformation
                    printDebugMessage opts "Creating output..."
                    generateOutput' conf mouts
        
        extractOrGenerate' conf fields obj req = do
            case lookupRequest req conf of
                Nothing -> do
                    printDebugMessage opts $ "Could not find request '" ++ req ++ "'."
                    return (req, Nothing)
                Just (_, _, extractor, generator) -> do
                    printDebugMessage opts "Found request. Looking at Force option..."
                    case optForce opts of
                        0 -> do
                            printDebugMessage opts "Force option 0: Only extraction"
                            extractionResult <- extract extractor fields
                            case extractionResult of
                                Nothing -> do
                                    printDebugMessage opts "Executing request failed"
                                    return (req, Nothing)
                                Just (jv, output) -> do
                                    printDebugMessage opts "Executing request succeeded."
                                    return $ (req, Just (jv, output))
                        1 -> do
                            printDebugMessage opts "Force option 1: Extraction, then generation if failed"
                            extractionResult <- extract extractor  fields
                            case extractionResult of
                                Nothing -> do
                                    generationResult <- generate generator obj
                                    case generationResult of
                                        Nothing -> do
                                            printDebugMessage opts "Executing request failed."
                                            return (req, Nothing)
                                        Just (jv, output) -> do
                                            printDebugMessage opts "Executing request succeeded."
                                            return $ (req, Just (jv, output))
                                Just (jv, output) -> do
                                    printDebugMessage opts "Executing request succeeded."
                                    return $ (req, Just (jv, output))
                        2 -> do
                            printDebugMessage opts "Force option 2: Only generation"
                            generationResult <- generate generator obj
                            case generationResult of
                                Nothing -> do
                                    printDebugMessage opts "Executing request failed"
                                    return (req, Nothing)
                                Just (jv, output) -> do
                                    printDebugMessage opts "Executing request succeeded."
                                    return $ (req, Just (jv, output))

        extract extractor fields = do
            printDebugMessage opts "Trying extraction..."
            extractionResult <- extractor opts fields
            case extractionResult of
                Nothing -> do
                    printDebugMessage opts "Extraction failed."
                    return Nothing
                Just (jv, output) -> do
                    printDebugMessage opts "Extraction succeeded."
                    return $ Just (jv, output)
        
        generate generator obj = do
            printDebugMessage opts "Trying generation..."
            generationResult <- generator opts obj
            case generationResult of
                Nothing -> do
                    printDebugMessage opts "Generation failed."
                    return Nothing
                Just (jv, output) -> do
                    printDebugMessage opts "Generation succeeded."
                    return $ Just (jv, output)
        
        generateOutput' conf mouts = do
            --outputs :: (String, Maybe String)
            case optOutput opts of
                "text" -> do
                    let outs = map (\(req, mout) -> req ++ ": " ++ maybe "FAILED" id mout) mouts
                    return $ OutputText (unlines outs)
                "json" -> do
                    let outs = map (\(req, mout) -> (req, JString (maybe "FAILED" id mout))) mouts
                    return $ OutputJSON (JObject outs)
        
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
        res <- getInfos2 opts obj args2
        printResult res
