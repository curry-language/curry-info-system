module CurryInfo.Analysis where

import CurryInfo.Types
import CurryInfo.Commands
import CurryInfo.Parser
    ( parseSafe, parseDeterministic, parseDemandness, parseIndeterministic, parseSolutionCompleteness, parseTermination
    , parseTotallyDefined
    )
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Paths
import CurryInfo.Checkout
import CurryInfo.Writer
import CurryInfo.Reader
import CurryInfo.ErrorMessage
import CurryInfo.JShow

import JSON.Data
import JSON.Parser (parseJSON)

import Data.List (init, find, intercalate)

import XML

-- Analysis

-- This action initiates a call to CASS to compute the given analysis for the given module.
-- The parser argument is for parsing the result of the analysis.
--analyseWithCASS :: Options -> String -> String -> Module -> String -> (String -> Maybe a) -> IO (Maybe ([(String, a)], a))
--analyseWithCASS opts path analysis m field parser = do
--        printDetailMessage opts $ "Starting analysis '" ++ analysis ++ "'..."
--        (_, output, _) <- runCmd opts (cmdCASS path analysis m)
--        printDetailMessage opts "Analysis finished."
--        printDebugMessage opts "Parsing result..."
--        let tmp = parseXmlString output
--        case tmp of
--            [e] -> do
--                printDebugMessage opts "Looking for results..."
--                case getXmlResults e of
--                    Nothing -> do
--                        printDebugMessage opts "Could not find analysis results."
--                        printDetailMessage opts "Analysis failed."
--                        return Nothing
--                    Just results -> do
--                        printDebugMessage opts "Results found. Looking for requested result..."
--                        case lookup field results of
--                            Nothing -> do
--                                printDebugMessage opts $ "Could not find entry with name '" ++ field ++ "'."
--                                printDetailMessage opts "Analysis failed."
--                                return Nothing
--                            Just result -> do
--                                printDetailMessage opts "Analysis succeeded."
--                                printDebugMessage opts $ "Result found: " ++ show result
--                                -- results :: [(String, String)]
--                                let otherResults = foldr (\(f, sr) xs -> maybe xs (\r -> (f, r):xs) (parser sr)) [] results
--                                return $ fmap (\r -> (otherResults, r)) (parser result)
--            _ -> do
--                printDebugMessage opts "Could not parse output. Expected XML."
--                printDebugMessage opts "Output:"
--                printDebugMessage opts output
--                printDetailMessage opts "Analysis failed."
--                return Nothing
--    where
--        getXmlResults :: XmlExp -> Maybe [(String, String)]
--        getXmlResults e = case tagOf e of
--            "results" -> mapM elemToResult (elemsOf e)
--            _ -> Nothing
--        
--        elemToResult :: XmlExp -> Maybe (String, String)
--        elemToResult e = do
--            case elemsOf e of
--                [_, n, r] -> do
--                    let name = textOf [n]
--                    let result = textOf [r]
--                    return (name, result)
--                _ -> Nothing

addInformation :: (ErrorMessage a, Path a) => Options -> (String -> a) -> JShower b -> (String -> Maybe b) -> String -> (String, String) -> IO ()
addInformation opts constructor jshower parser field (name, sresult) = do
    case parser sresult of
        Nothing -> do
            printDebugMessage opts $ "Parsing failed for result of '" ++ name ++ "'."
        Just result -> do
            let obj = constructor name
            initialize obj
            mfields <- readInformation opts obj
            case mfields of
                Nothing -> do
                    printDebugMessage opts $ errorMessage obj
                Just fields -> do
                    let newInformation = [(field, jshower result)] <+> fields
                    writeInformation obj newInformation

--analyseWithCASS :: Options -> String -> String -> Module -> String -> (String -> Maybe a) -> IO (Maybe ([(String, a)], a))
analyseWithCASS :: (ErrorMessage a, Path a) => Options -> Package -> Version -> Module -> String -> String -> String -> (String -> Maybe b) -> (String -> a) -> JShower b -> IO (Maybe b)
analyseWithCASS opts pkg vsn m name analysis field parser constructor jshower = do
        printDetailMessage opts $ "Starting analysis '" ++ analysis ++ "'..."
        mpath <- checkoutIfMissing opts pkg vsn
        case mpath of
            Nothing -> do
                printDetailMessage opts "Analysis failed."
                return Nothing
            Just path -> do
                (_, output, _) <- runCmd opts (cmdCASS path analysis m)
                printDetailMessage opts "Analysis finished."
                printDebugMessage opts "Parsing anaylsis output..."
                case parseXmlString output of
                    [e] -> do
                        printDebugMessage opts "Looking for results..."
                        case getXmlResults e of
                            Nothing -> do
                                printDebugMessage opts "Could not find analysis results."
                                printDetailMessage opts "Analysis failed."
                                return Nothing
                            Just results -> do
                                printDebugMessage opts "Results found. Looking for requested result..."
                                case lookup name results of
                                    Nothing -> do
                                        printDebugMessage opts $ "Could not find entry with name '" ++ name ++ "'."
                                        printDetailMessage opts "Analysis failed."
                                        return Nothing
                                    Just result -> do
                                        printDetailMessage opts "Analysis succeeded."
                                        printDebugMessage opts $ "Result found: " ++ show result

                                        printDebugMessage opts "Writing all results in files..."
                                        mapM (addInformation opts constructor jshower parser field) results

                                        return (parser result)
                    _ -> do
                        printDebugMessage opts "Could not parse output. Expected XML."
                        printDebugMessage opts "Output:"
                        printDebugMessage opts output
                        printDetailMessage opts "Analysis failed."
                        return Nothing
    where
        getXmlResults :: XmlExp -> Maybe [(String, String)]
        getXmlResults e = case tagOf e of
            "results" -> mapM elemToResult (elemsOf e)
            _ -> Nothing
        
        elemToResult :: XmlExp -> Maybe (String, String)
        elemToResult e = do
            case elemsOf e of
                [_, n, r] -> return (textOf [n], textOf [r])
                _ -> Nothing

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis for the given module in
-- the given path.
analyseSafeModule :: Options -> Package -> Version -> Module -> IO (Maybe Safe)
analyseSafeModule opts pkg vsn m = do
    analyseWithCASS opts pkg vsn m m "UnsafeModule" "safe" parseSafe (CurryModule pkg vsn) jsModuleSafe

-- This action initiates a call to CASS to compute the 'Deterministic' analysis for the given module in
-- the given path.
analyseDeterministic :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Deterministic)
analyseDeterministic opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Deterministic" "deterministic" parseDeterministic (CurryOperation pkg vsn m) jsOperationDeterministic

-- This action initiates a call to CASS to compute the 'Demand' analysis for the given module in
-- the given path.
analyseDemandness :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Demandness)
analyseDemandness opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Demand" "demandness" parseDemandness (CurryOperation pkg vsn m) jsOperationDemandness

-- This action initiates a call to CASS to compute the 'Indeterministic' analysis for the given module in
-- the given path.
analyseIndeterministic :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Indeterministic)
analyseIndeterministic opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Indeterministic" "indeterministic" parseIndeterministic (CurryOperation pkg vsn m) jsOperationIndeterministic

-- This action initiates a call to CASS to compute the 'SolComplete' analysis for the given module in
-- the given path.
analyseSolutionCompleteness :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe SolutionCompleteness)
analyseSolutionCompleteness opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "SolComplete" "solutionCompleteness" parseSolutionCompleteness (CurryOperation pkg vsn m) jsOperationSolutionCompleteness

-- This action initiates a call to CASS to compute the 'Terminating' analysis for the given module in
-- the given path.
analyseTermination :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Termination)
analyseTermination opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Terminating" "termination" parseTermination (CurryOperation pkg vsn m) jsOperationTermination

-- This action initiates a call to CASS to compute the 'Total' analysis for the given module in
-- the given path.
analyseTotallyDefined :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe TotallyDefined)
analyseTotallyDefined opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Total" "totallyDefined" parseTotallyDefined (CurryOperation pkg vsn m) jsOperationTotallyDefined
