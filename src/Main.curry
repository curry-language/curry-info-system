module Main where

import CurryInfo.JParser (JParser)
import CurryInfo.JPretty (JPretty, jsonOutput)
import CurryInfo.Configuration
import CurryInfo.Paths (Path, initialize, packagesPath, getDirectoryPath, getJSONPath)
import CurryInfo.Types
import CurryInfo.Reader (Reader, readInformation)
import CurryInfo.Writer (Writer, writeInformation)
import CurryInfo.ErrorMessage (ErrorMessage, errorMessage)
import CurryInfo.Options
import CurryInfo.Verbosity (printLine, printDebugMessage)

import JSON.Parser (parseJSON)
import JSON.Pretty (ppJSON)
import JSON.Data

import Data.Maybe (catMaybes, isJust)
import Data.List (nubBy)

import System.Environment (getArgs)
import System.Process (exitWith)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectory, removeFile, getDirectoryContents)
import System.FilePath ((</>))

import Control.Monad (unless, zipWithM, filterM)

-- This action extracts or generates the requested information for the given object.
getInfos :: Options -> [(String, String)] -> [String] -> IO Output
getInfos opts location requests = do
    printLine opts
    printDebugMessage opts "Checking structure of the request..."
    case location of
        [("packages", pkg)]                                                         -> do
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
        _ -> return $ OutputError $ show location ++ " does not match any pattern"
    where
        getInfos' conf input = do
            printLine opts
            printDebugMessage opts "Initializing Input..."
            initialize input
            printDebugMessage opts "Reading current information..."
            result <- readInformation opts input
            case result of
                Nothing -> do
                    printDebugMessage opts "Reading information failed."
                    return $ OutputError $ errorMessage input
                Just infos -> do
                    let infos' = map (\i -> (fieldName i, i)) infos
                    printDebugMessage opts "Reading information succeeded."
                    printDebugMessage opts "Extracting/Generating requested information..."
                    results <- mapM (extractOrGenerate opts conf input infos') requests

                    let newInformation = catMaybes results <+> infos'
                    printDebugMessage opts "Overwriting with updated information..."
                    writeInformation input (map snd newInformation)
                    printDebugMessage opts "Creating output..."
                    --return $ generateOutput opts requests results
                    generateOutput opts conf requests (map (fmap snd) results) 
        
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

-- This action extracts or generates the requested information for the input, depending on whether the information
-- already exists or not.
extractOrGenerate :: Options -> Configuration a b -> a -> [(String, b)] -> String -> IO (Maybe (String, b))
extractOrGenerate opts conf input infos request = do
    printDebugMessage opts $ "Looking up extractor and generator for request '" ++ request ++ "'..."
    case optForce opts of
        2 -> do
            printDebugMessage opts "Force options is 2. Only generating information."
            printDebugMessage opts "Looking for generator..."
            case findGenerator request conf of
                Nothing -> do
                    printDebugMessage opts $ "Could not find generator for request '" ++ request ++ "'."
                    return Nothing
                Just generator -> do
                    printDebugMessage opts "Generator found."
                    generator opts input
        1 -> do
            printDebugMessage opts "Force option is 1. Extracting or generating information."
            printDebugMessage opts "Looking for extractor and generator..."
            case findGenerator request conf of
                Nothing -> do
                    printDebugMessage opts $ "Could not find generator for request '" ++ request ++ "'."
                    return Nothing
                Just generator -> do
                    printDebugMessage opts "Generator found."
                    --maybe (generator opts input) (return . Just) (extractor infos)
                    maybe (generator opts input) (return . Just) ((,) request <$> lookup request infos)
        0 -> do
            printDebugMessage opts "Force option is 0. Only extracting information."
            return $ fmap (\i -> (request, i)) (lookup request infos)
    {-
    case lookup request conf of
        Nothing                     -> do
            printDebugMessage opts "Entry not found in configuration."
            return Nothing
        Just (_, extractor, generator) -> do
            printDebugMessage opts "Extractor and Generator found."
            printDebugMessage opts "Looking at force option..."
            case optForce opts of
                2 -> do
                    printDebugMessage opts "Force option is 2. Generating information..."
                    generator opts input
                1 -> do
                    printDebugMessage opts "Force option is 1. Generating/Extracting information..."
                    maybe (generator opts input) (return . Just) (extractor infos)
                0 -> do
                    printDebugMessage opts "Force option is 0. Extracting information..."
                    return $ extractor infos
    -}

{-
generateOutput :: JPretty a => [String] -> [Maybe a] -> Output
generateOutput fields results =
    let outputs = zipWith (\f r -> maybe (f, "failed") jsonOutput r) fields results
    in OutputText $ unlines $ map (\(f, m) -> f ++ ": " ++ m) outputs
-}

-- This function generates an output for the fields and respective results of extracting or generating.
generateOutput :: Options -> Configuration a b -> [String] -> [Maybe b] -> IO Output
generateOutput opts conf fields results = do
        outputs <- zipWithM (generateOutput') fields results
        let output = unlines outputs
        case optOutput opts of
            "text"  -> return (OutputText output)
            "json"  -> return (OutputJSON $ JObject (zipWith generateField fields outputs))
            format  -> return (OutputError $ "Unknown output format: " ++ format)
    where
        generateOutput' f mr = do
            case mr of
                Nothing -> return (f ++ ": " ++ "FAILED TO EXTRACT/GENERATE")
                Just r -> do
                    case findPrinter f conf of
                        Nothing -> return (f ++ ": " ++ "FAILED TO FIND PRINTER")
                        Just p -> do
                            msg <- p opts r
                            return (f ++ ": " ++ msg)
        generateField :: String -> String -> (String, JValue)
        generateField field output = (field, JString output)
        

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

cleanObject :: Options -> [(String, String)] -> IO ()
cleanObject opts obj = do
        case obj of
            [] -> cleanAll
            [("packages", pkg)] -> let x = CurryPackage pkg in cleanJSON x >> cleanDirectory x
            [("packages", pkg), ("versions", vsn)] -> let x = CurryVersion pkg vsn in cleanJSON x >> cleanDirectory x
            [("packages", pkg), ("versions", vsn), ("modules", m)] -> let x = CurryModule pkg vsn m in cleanJSON x >> cleanDirectory x
            [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)] -> let x = CurryType pkg vsn m t in cleanJSON x
            [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)] -> let x = CurryTypeclass pkg vsn m c in cleanJSON x
            [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)] -> let x = CurryOperation pkg vsn m o in cleanJSON x
            _ -> printDebugMessage opts $ show obj ++ " does not match any pattern"
    where
        cleanJSON :: Path a => a -> IO ()
        cleanJSON obj' = do
            path <- getJSONPath obj'
            b <- doesFileExist path
            case b of
                False -> do
                    printDebugMessage opts $ "json file does not exist: " ++ path ++ "\nNot cleaning up json file."
                    return ()
                True -> do
                    printDebugMessage opts $ "json file exists. Cleaning up json file."
                    removeFile path

        cleanDirectory :: Path a => a -> IO ()
        cleanDirectory obj' = do
            path <- getDirectoryPath obj'
            b <- doesDirectoryExist path
            case b of
                False -> do
                    printDebugMessage opts $ "directory does not exist: " ++ path ++ "\nNot cleaning up directory."
                    return ()
                True -> do
                    printDebugMessage opts $ "directory exists. Cleaning up directory."
                    deleteDirectory path

        cleanAll :: IO ()
        cleanAll = do
            path <- packagesPath
            b <- doesDirectoryExist path
            case b of
                False -> do
                    printDebugMessage opts $ "directory does not exist: " ++ path ++ "\nNot cleaning up directory."
                    return ()
                True -> do
                    printDebugMessage opts $ "directory exists. Cleaning up directory."
                    deleteDirectory path
        
        deleteDirectory :: String -> IO ()
        deleteDirectory path = do
            printDebugMessage opts $ "Deleting directory: " ++ path

            contents <- fmap ((map (\p -> path </> p)) . (filter (\p -> p /= "." && p /= ".."))) (getDirectoryContents path)
            printDebugMessage opts $ "Found contents: " ++ show contents

            dirs <- filterM doesDirectoryExist contents
            printDebugMessage opts $ "Subdirectories found: " ++ show dirs

            files <- filterM doesFileExist contents
            printDebugMessage opts $ "Files found: " ++ show files

            printDebugMessage opts "Deleting subdirectories..."
            mapM deleteDirectory dirs

            printDebugMessage opts "Deleting files..."
            mapM removeFile files

            printDebugMessage opts "Deleting directory..."
            removeDirectory path
            
            printDebugMessage opts $ "Finished deleting directory: " ++ path

main :: IO ()
main = do
        args <- getArgs
        (opts, args2) <- processOptions "" args

        let pkg = extractOpt "packages"     (optPackage opts)

        let vsn = extractOpt "versions"     (optVersion opts)
        let m   = extractOpt "modules"      (optModule opts)
        let t   = extractOpt "types"        (optType opts)
        let c   = extractOpt "typeclasses"  (optTypeclass opts)
        let op  = extractOpt "operations"   (optOperation opts)

        let obj = catMaybes [pkg, vsn, m, t, c, op]

        if args2 == ["clean"]
            then cleanObject opts obj
            else do
                unless (isJust pkg) (putStrLn "Package name is required" >> exitWith 1)
                res <- getInfos opts obj args2
                printResult res
    where
        extractOpt :: String -> Maybe String -> Maybe (String, String)
        extractOpt tag = fmap (\x -> (tag, x))
