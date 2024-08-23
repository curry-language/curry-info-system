module CurryAnalysisInfrastructure.Analysis where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Commands
import CurryAnalysisInfrastructure.Parser
    ( parseSafe, parseDeterministic, parseDemandness, parseIndeterministic, parseSolutionCompleteness, parseTermination
    , parseTotallyDefined
    )
import CurryAnalysisInfrastructure.Verbosity (printLine, printDebugMessage)

import JSON.Data
import JSON.Parser (parseJSON)

import Data.List (init, find, intercalate)

import XML

-- Analysis

{-
-- This operation looksup a field with a specific name in a javascript object. If not such field can be found or
-- the javascript value is not a javascript object, then it will return Nothing.
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

-- This action initiates a call to CASS to compute the given analysis for the given module.
-- The parser argument is for parsing the result of the analysis.
analyseWithCASS :: Options -> String -> String -> Module -> String -> (String -> Maybe a) -> IO (Maybe a)
analyseWithCASS opts path analysis m field parser = do
    printLine opts
    printDebugMessage opts $ "Starting analysis '" ++ analysis ++ "'..."
    (_, output, _) <- runCmd opts (cmdCASS path analysis m)
    printDebugMessage opts "Analysis finished."
    printDebugMessage opts "Parsing result..."
    let tmp = parseJSON (init output)
    case tmp of
        Just (JArray js) -> do
            printDebugMessage opts "Looking for result field..."
            case findField js field of
                Just result -> do
                    printDebugMessage opts "Analysis succeeded."
                    return $ parser result
                Nothing -> do
                    printDebugMessage opts $ "Could not find entry with name '" ++ field ++ "'."
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        _ -> do
            printDebugMessage opts "Output did not match expected format. Expected array."
            printDebugMessage opts "Output:"
            printDebugMessage opts output
            printDebugMessage opts "Analysis failed."
            return Nothing
-}

-- This action initiates a call to CASS to compute the given analysis for the given module.
-- The parser argument is for parsing the result of the analysis.
analyseWithCASS :: Options -> String -> String -> Module -> String -> (String -> Maybe a) -> IO (Maybe a)
analyseWithCASS opts path analysis m field parser = do
        printLine opts
        printDebugMessage opts $ "Starting analysis '" ++ analysis ++ "'..."
        (_, output, _) <- runCmd opts (cmdCASS path analysis m)
        printDebugMessage opts "Analysis finished."
        printDebugMessage opts "Parsing result..."
        let tmp = parseXmlString output
        case tmp of
            [e] -> do
                printDebugMessage opts "Looking for results..."
                case getXmlResults e of
                    Nothing -> do
                        printDebugMessage opts "Could not find analysis results."
                        printDebugMessage opts "Analysis failed."
                        return Nothing
                    Just es -> do
                        printDebugMessage opts "Results field found. Looking for requested result..."
                        case getXmlResult field es of
                            Nothing -> do
                                printDebugMessage opts $ "Could not find entry with name '" ++ field ++ "'."
                                printDebugMessage opts "Analysis failed."
                                return Nothing
                            Just result -> do
                                printDebugMessage opts "Analysis succeeded."
                                printDebugMessage opts $ "Result found: " ++ show result
                                return $ parser result
            _ -> do
                printDebugMessage opts "Could not parse output. Expected XML."
                printDebugMessage opts "Output:"
                printDebugMessage opts output
                printDebugMessage opts "Analysis failed."
                return Nothing
    where
        getXmlResults :: XmlExp -> Maybe [XmlExp]
        getXmlResults e = if tagOf e == "results" then Just (elemsOf e) else Nothing

        getXmlResult :: String -> [XmlExp] -> Maybe String
        getXmlResult field es = do
                results <- mapM elemToResult es
                lookup field results

        elemToResult :: XmlExp -> Maybe (String, String)
        elemToResult e = do
            case elemsOf e of
                [_, n, r] -> do
                    let name = textOf [n]
                    let result = textOf [r]
                    return (name, result)
                _ -> Nothing

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis for the given module in
-- the given path.
analyseSafeModule :: Options -> String -> Module -> IO (Maybe Safe)
analyseSafeModule opts path m = do
    analyseWithCASS opts path "UnsafeModule" m m parseSafe

-- This action initiates a call to CASS to compute the 'Deterministic' analysis for the given module in
-- the given path.
analyseDeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Deterministic)
analyseDeterministic opts path m op = do
    analyseWithCASS opts path "Deterministic" m op parseDeterministic

-- This action initiates a call to CASS to compute the 'Demand' analysis for the given module in
-- the given path.
analyseDemandness :: Options -> String -> Module -> Operation -> IO (Maybe Demandness)
analyseDemandness opts path m op = do
    analyseWithCASS opts path "Demand" m op parseDemandness

-- This action initiates a call to CASS to compute the 'Indeterministic' analysis for the given module in
-- the given path.
analyseIndeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Indeterministic)
analyseIndeterministic opts path m op = do
    analyseWithCASS opts path "Indeterministic" m op parseIndeterministic

-- This action initiates a call to CASS to compute the 'SolComplete' analysis for the given module in
-- the given path.
analyseSolutionCompleteness :: Options -> String -> Module -> Operation -> IO (Maybe SolutionCompleteness)
analyseSolutionCompleteness opts path m op = do
    analyseWithCASS opts path "SolComplete" m op parseSolutionCompleteness

-- This action initiates a call to CASS to compute the 'Terminating' analysis for the given module in
-- the given path.
analyseTermination :: Options -> String -> Module -> Operation -> IO (Maybe Termination)
analyseTermination opts path m op = do
    analyseWithCASS opts path "Terminating" m op parseTermination

-- This action initiates a call to CASS to compute the 'Total' analysis for the given module in
-- the given path.
analyseTotallyDefined :: Options -> String -> Module -> Operation -> IO (Maybe TotallyDefined)
analyseTotallyDefined opts path m op = do
    analyseWithCASS opts path "Total" m op parseTotallyDefined