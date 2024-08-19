module Main where

import CurryAnalysisInfrastructure.JParser (JParser)
import CurryAnalysisInfrastructure.JPretty (JPretty, jsonOutput)
import CurryAnalysisInfrastructure.Configuration
import CurryAnalysisInfrastructure.Paths (Path, initialize)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Reader (Reader, readInformation)
import CurryAnalysisInfrastructure.Writer (Writer, writeInformation)
import CurryAnalysisInfrastructure.ErrorMessage (ErrorMessage, errorMessage)
import CurryAnalysisInfrastructure.Options
import CurryAnalysisInfrastructure.Verbosity (printLine, printDebugMessage)

import JSON.Parser (parseJSON)
import JSON.Pretty (ppJSON)

import Data.Maybe (catMaybes, isJust)
import Data.List (nubBy)

import System.Environment (getArgs)
import System.Process (exitWith)

import Control.Monad (unless)

-- This action extracts or generates the requested information for the given object.
getInfos :: Options -> [(String, String)] -> [String] -> IO Output
getInfos opts location requests = do
    printLine opts
    printDebugMessage opts "Checking structure of the request..."
    case location of
        [("packages", pkg)]                                                         -> do
            printDebugMessage opts "Structure matches Package."
            getInfos' opts packageConfiguration (CurryPackage pkg) requests
        [("packages", pkg), ("versions", vsn)]                                      -> do
            printDebugMessage opts "Structure matches Version."
            getInfos' opts versionConfiguration (CurryVersion pkg vsn) requests
        [("packages", pkg), ("versions", vsn), ("modules", m)]                      -> do
            printDebugMessage opts "Structure matches Module."
            result <- checkModuleExists pkg vsn m
            case result of
                False -> do
                    printDebugMessage opts "Module does not exist or is not exported."
                    return $ OutputError $ "Module " ++ m ++ " is not exported in version " ++ vsn ++ " of package " ++ pkg ++ "."
                True  -> do
                    printDebugMessage opts "Module exists."
                    getInfos' opts moduleConfiguration (CurryModule pkg vsn m) requests
        [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)]        -> do
            printDebugMessage opts "Structure matches Type"
            result <- checkTypeExists pkg vsn m t
            case result of
                False -> do
                    printDebugMessage opts "Type does not exist or is not exported."
                    return $ OutputError $ "Type " ++ t ++ " is not exported in module " ++ m ++ " in version " ++ vsn ++ " of package " ++ pkg ++ "."
                True  -> do
                    printDebugMessage opts "Type exists."
                    getInfos' opts typeConfiguration (CurryType pkg vsn m t) requests
        [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)]  -> do
            printDebugMessage opts "Structure matches Typeclass"
            result <- checkTypeclassExists pkg vsn m c
            case result of
                False -> do
                    printDebugMessage opts "Typeclass does not exist or is not exported."
                    return $ OutputError $ "Typeclass " ++ c ++ " is not exported in module " ++ m ++ " in version " ++ vsn ++ " of package " ++ pkg ++ "."
                True  -> do
                    printDebugMessage opts "Typeclass exists."
                    getInfos' opts typeclassConfiguration (CurryTypeclass pkg vsn m c) requests
        [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)]   -> do
            printDebugMessage opts "Structure matches Operation."
            result <- checkOperationExists pkg vsn m o
            case result of
                False -> do
                    printDebugMessage opts "Operation does not exist or is not exported."
                    return $ OutputError $ "Operation " ++ o ++ " is not exported in module " ++ m ++ " in version " ++ vsn ++ " of package " ++ pkg ++ "."
                True  -> do
                    printDebugMessage opts "Operation exists."
                    getInfos' opts operationConfiguration (CurryOperation pkg vsn m o) requests
        _ -> return $ OutputError $ show location ++ " does not match any pattern"
    where
        getInfos' :: (Path a, ErrorMessage a, EqInfo b, JParser b, JPretty b) => Options -> Configuration a b -> a -> [String] -> IO Output
        getInfos' opts' conf input requests' = do
            printLine opts
            printDebugMessage opts "Initializing Input..."
            initialize input
            printDebugMessage opts "Reading current information..."
            result <- readInformation opts' input
            case result of
                Nothing -> do
                    printDebugMessage opts "Reading information failed."
                    return $ OutputError $ errorMessage input
                Just infos -> do
                    printDebugMessage opts "Reading information succeeded."
                    printDebugMessage opts "Extracting/Generating requested information..."
                    results <- mapM (extractOrGenerate opts' conf input infos) requests'

                    let newInformation = catMaybes results <+> infos
                    printDebugMessage opts "Overwriting with updated information..."
                    writeInformation input newInformation
                    printDebugMessage opts "Creating output..."
                    return $ generateOutput requests' results
        
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
extractOrGenerate :: Options -> Configuration a b -> a -> [b] -> String -> IO (Maybe b)
extractOrGenerate opts conf input infos request = do
    printDebugMessage opts $ "Looking up extractor and generator for request '" ++ request ++ "'..."
    case lookup request conf of
        Nothing                     -> do
            printDebugMessage opts "Entry not found in configuration."
            return Nothing
        Just (extractor, generator) -> do
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

-- This function generates an output for the fields and respective results of extracting or generating.
generateOutput :: JPretty a => [String] -> [Maybe a] -> Output
generateOutput fields results =
    let outputs = zipWith (\f r -> maybe (f, "failed") jsonOutput r) fields results
    in OutputText $ unlines $ map (\(f, m) -> f ++ ": " ++ m) outputs

-- This operator combines two lists and excludes all dublicates. The first list should contain the newer information
-- to get an updated list.
(<+>) :: EqInfo a => [a] -> [a] -> [a]
info1 <+> info2 = nubBy sameInfo (info1 ++ info2)

printResult :: Output -> IO ()
printResult (OutputText txt) = do
    putStrLn ""
    putStrLn "Finished with OutputText"
    putStrLn txt
printResult (OutputError err) = do
    putStrLn ""
    putStrLn "Finished with OutputError"
    putStrLn err

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
        printResult res
    where
        extractOpt :: String -> Maybe String -> Maybe (String, String)
        extractOpt tag = fmap (\x -> (tag, x))
