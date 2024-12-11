-----------------------------------------------------------------------------
--- This modules defines operations to create outputs and process requests
--- to get information about objects.
-----------------------------------------------------------------------------

module CurryInfo.Information where

import CurryInfo.Configuration
import CurryInfo.Paths (Path, initialize, index, getJSONPath)
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.Options (getObject, queryOptions)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Generator (readPackageJSON, getExportedModules, readPackageModules)
import CurryInfo.Helper (InformationResult(..), information)

import JSON.Pretty (ppJSON)
import JSON.Data
import JSON.Parser (parseJSON)

import Data.Maybe (isJust)
import Data.Either (partitionEithers)
import Data.Char (toLower)

import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Directory (doesDirectoryExist, doesFileExist)

import Control.Monad (unless, zipWithM, when)

--- This action prints the given output to stdout and also returns the string as result.
printResult :: Output -> IO String
printResult (OutputText txt) = putStrLn txt >> return txt
printResult (OutputJSON jv) = let txt = ppJSON jv in putStrLn txt >> return txt
printResult (OutputTerm ts) = let txt = show ts in putStrLn txt >> return txt
printResult (OutputError err) = let txt = "Error: " ++ err in putStrLn txt >> return txt

--- This action returns a failed output with the given error message.
generateOutputError :: Options -> String -> IO Output
generateOutputError opts err = do
    printDetailMessage opts err
    case optOutput opts of
        OutText -> return $ OutputText err
        OutJSON -> return $ OutputJSON (JString err)
        OutTerm -> return $ OutputTerm []

--- This actions process the given requests for the given object and returns the output.
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
                    generateOutputError opts err
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
                    generateOutputError opts err
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
                    generateOutputError opts err
                True  -> do
                    printDetailMessage opts "Checked Module: exists."
                    case (optAllTypes opts, optAllTypeclasses opts, optAllOperations opts) of
                        (True, _, _) -> do
                            mts <- queryAllTypes pkg vsn m
                            case mts of
                                Nothing -> do
                                    generateOutputError opts "Could not find types"
                                Just ts -> do
                                    outs <- mapM (getInfos' typeConfiguration) (map (CurryType pkg vsn m) ts)
                                    let out = combineOutput outs
                                    return out
                        (_, True, _) -> do
                            mcs <- queryAllTypeclasses pkg vsn m
                            case mcs of
                                Nothing -> do
                                    generateOutputError opts "Could not find typeclasses"
                                Just cs -> do
                                    outs <- mapM (getInfos' typeclassConfiguration) (map (CurryTypeclass pkg vsn m) cs)
                                    let out = combineOutput outs
                                    return out
                        (_, _, True) -> do
                            mos <- queryAllOperations pkg vsn m
                            case mos of
                                Nothing -> do
                                    generateOutputError opts "Could not find operations."
                                Just os -> do
                                    outs <- mapM (getInfos' operationConfiguration) (map (CurryOperation pkg vsn m) os)
                                    let out = combineOutput outs
                                    return out
                        (False, False, False) -> do
                            getInfos' moduleConfiguration obj
        [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)]        -> do
            printStatusMessage opts "Structure matches Type"
            let obj = CurryType pkg vsn m t
            result <- checkTypeExists obj
            case result of
                False -> do
                    let err = "Type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "' is not exported."
                    printDetailMessage opts err
                    generateOutputError opts err
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
                    generateOutputError opts err
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
                    generateOutputError opts err
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
                                    results <- zipWithM (\(_, _, extractor, _) fieldName -> fmap (\x -> (fieldName, x)) (extract extractor fields)) allReqs fieldNames :: IO [(String, Maybe (JValue, String))]
                                    let results' = map (\(r, mr) -> (r, maybe InformationExtractionFailed (uncurry InformationResult) mr)) results
                                    let output = createOutput obj results' :: Output
                                    return output
                        False -> do
                            printDetailMessage opts "Extracting/Generating requested information..."
                            results <- mapM (extractOrGenerate conf fields obj) (map (map toLower) reqs) :: IO [(String, InformationResult)]

                            let newInformation = createNewInformation results
                            printDebugMessage opts "Overwriting with updated information..."
                            writeInformation obj (newInformation <+> fields)
                            printDetailMessage opts "Overwriting finished."

                            let output = createOutput obj results :: Output
                            return output
        
        queryAllTypes :: Package -> Version -> Module -> IO (Maybe [Type])
        queryAllTypes pkg vsn m = query [("packages", pkg), ("versions", vsn), ("modules", m)] "types"

        queryAllTypeclasses :: Package -> Version -> Module -> IO (Maybe [Typeclass])
        queryAllTypeclasses pkg vsn m = query [("packages", pkg), ("versions", vsn), ("modules", m)] "typeclasses"

        queryAllOperations :: Package -> Version -> Module -> IO (Maybe [Operation])
        queryAllOperations pkg vsn m = query [("packages", pkg), ("versions", vsn), ("modules", m)] "operations"
        
        combineOutput :: [Output] -> Output
        combineOutput outs = case optOutput opts of
            OutText -> OutputText (unlines (map fromOutputText outs))
            OutJSON -> OutputJSON (JArray (map fromOutputJSON outs))
            OutTerm -> OutputTerm (concatMap fromOutputTerm outs)
        
        fromOutputText :: Output -> String
        fromOutputText (OutputText txt) = txt

        fromOutputJSON :: Output -> JValue
        fromOutputJSON (OutputJSON jv) = jv

        fromOutputTerm :: Output -> [(String, String)]
        fromOutputTerm (OutputTerm ts) = ts

        createNewInformation :: [(String, InformationResult)] -> [(String, JValue)]
        createNewInformation = foldr (\(r, ir) acc -> information acc (const acc) (\jv _ -> (r, jv):acc) ir) []

        -- generate output for a single entity?
        outputSingleEntity = not
          (optAllTypes opts || optAllTypeclasses opts || optAllOperations opts)
        
        createOutput :: ToString a => a -> [(String, InformationResult)] -> Output
        createOutput obj results = case optOutput opts of
            OutText -> OutputText $ unlines $
                         (if outputSingleEntity then [] else [toString obj]) ++
                         map (\(r, ir) -> r ++ ": " ++ 
                                          information "?" id (flip const) ir)
                             results
            OutJSON -> OutputJSON (JObject [("object", (JString . toString) obj), ("results", JObject (map (\(r, ir) -> (r, information JNull JString (\_ s -> JString s) ir)) results))])
            OutTerm -> OutputTerm [(toString obj, show (map (\(r, ir) -> (r, information "?" id (flip const) ir)) results))]

        extractOrGenerate :: ErrorMessage a => Configuration a -> [(String, JValue)] -> a -> String -> IO (String, InformationResult)
        extractOrGenerate conf fields obj req = do
            printStatusMessage opts $ "\nProcessing request '" ++ req ++ "'..."
            case lookupRequest req conf of
                Nothing -> do
                    let msg = errorRequest obj req
                    printDetailMessage opts $ msg
                    return (req, InformationError "REQUEST NOT FOUND")
                Just (_, _, extractor, generator) -> do
                    printDetailMessage opts "Found request. Looking at Force option..."
                    case optForce opts of
                        0 -> do
                            printDebugMessage opts "Force option 0: Only extraction"
                            extractionResult <- extract extractor fields
                            case extractionResult of
                                Nothing -> do
                                    printDetailMessage opts "Executing request failed"
                                    return (req, InformationExtractionFailed)
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return (req, InformationResult jv output)
                        1 -> do
                            printDebugMessage opts "Force option 1: Extraction, then generation if failed"
                            extractionResult <- extract extractor fields
                            case extractionResult of
                                Nothing -> do
                                    generationResult <- generate generator obj
                                    case generationResult of
                                        Nothing -> do
                                            printDetailMessage opts "Executing request failed."
                                            return (req, InformationError "EXTRACTING AND GENERATING FAILED")
                                        Just (jv, output) -> do
                                            printDetailMessage opts "Executing request succeeded."
                                            return (req, InformationResult jv output)
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return (req, InformationResult jv output)
                        2 -> do
                            printDebugMessage opts "Force option 2: Only generation"
                            generationResult <- generate generator obj
                            case generationResult of
                                Nothing -> do
                                    printDetailMessage opts "Executing request failed"
                                    return (req, InformationError "GENERATING FAILED")
                                Just (jv, output) -> do
                                    printDetailMessage opts "Executing request succeeded."
                                    return (req, InformationResult jv output)
                        v -> do
                            let msg = "INVALID FORCE OPTION: " ++ show v
                            printDebugMessage opts msg
                            return (req, InformationError msg)

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
                    res <- query [("packages", pkg), ("versions", vsn), ("modules", m)] "types"
                    case res of
                        Nothing -> return False
                        Just ts -> return $ elem t ts

        checkTypeclassExists :: CurryTypeclass -> IO Bool
        checkTypeclassExists obj@(CurryTypeclass pkg vsn m c) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
                    res <- query [("packages", pkg), ("versions", vsn), ("modules", m)] "typeclasses"
                    case res of
                        Nothing -> return False
                        Just cs -> return $ elem c cs

        checkOperationExists :: CurryOperation -> IO Bool
        checkOperationExists obj@(CurryOperation pkg vsn m o) = do
            jpath <- getJSONPath obj
            b1 <- doesFileExist jpath
            case b1 of
                True -> return True
                False -> do
                    res <- query [("packages", pkg), ("versions", vsn), ("modules", m)] "operations"
                    case res of
                        Nothing -> return False
                        Just os -> return $ elem o os

        query :: Read a => [(String, String)] -> String -> IO (Maybe a)
        query obj req = do
            res <- getInfos queryOptions obj [req] :: IO Output
            case res of
                -- OutputTerm [("obj", [("req", "res")])]
                OutputTerm [(_, x)] -> case lookup req (read x) of
                    Nothing -> return Nothing
                    Just y -> return (Just (read y))
                _ -> return Nothing