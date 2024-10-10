module CurryInfo.Analysis where

import CurryInfo.Types
import CurryInfo.Commands
import CurryInfo.Parser
    ( parseUnsafe, parseDeterministic, parseDemand, parseIndeterministic, parseSolComplete, parseTerminating
    , parseTotal
    )
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Paths
import CurryInfo.Checkout
import CurryInfo.Writer
import CurryInfo.Reader
import CurryInfo.JShow

import JSON.Data
import JSON.Parser (parseJSON)

import XML

-- Analysis

-- This action initiates a call to CASS to compute the given analysis for the given module.
-- The parser argument is for parsing the result of the analysis.
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
                printDebugMessage opts "Parsing analysis output..."
                case parseXmlString output of
                    [e] -> do
                        printDebugMessage opts "Looking for results..."
                        case getXmlResults e of
                            Nothing -> do
                                printDebugMessage opts "Could not find analysis results."
                                printDetailMessage opts "Analysis failed."
                                return Nothing
                            Just results -> do
                                printDebugMessage opts "Writing all results in files..."
                                mapM addInformation results

                                printDebugMessage opts "Results found. Looking for requested result..."
                                case lookup name results of
                                    Nothing -> do
                                        printDebugMessage opts $ "Could not find entry with name '" ++ name ++ "'."
                                        printDetailMessage opts "Analysis failed."
                                        return Nothing
                                    Just result -> do
                                        printDetailMessage opts "Analysis succeeded."
                                        printDebugMessage opts $ "Result found: " ++ show result

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

        addInformation :: (String, String) -> IO ()
        addInformation (sname, sresult) = do
            case parser sresult of
                Nothing -> do
                    printDebugMessage opts $ "Parsing failed for result of '" ++ sname ++ "'."
                Just result -> do
                    let obj = constructor sname
                    initialize obj
                    mfields <- readInformation opts obj
                    case mfields of
                        Nothing -> do
                            printDebugMessage opts $ errorReading obj
                        Just fields -> do
                            let newInformation = [(field, jshower result)] <+> fields
                            writeInformation obj newInformation

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis for the given module in
-- the given path.
analyseUnsafeModuleWithCASS :: Options -> Package -> Version -> Module -> IO (Maybe Unsafe)
analyseUnsafeModuleWithCASS opts pkg vsn m = do
    analyseWithCASS opts pkg vsn m m "UnsafeModule" "cass-unsafemodule" parseUnsafe (CurryModule pkg vsn) jsModuleUnsafeModule

-- This action initiates a call to CASS to compute the 'Deterministic' analysis for the given module in
-- the given path.
analyseDeterministicWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Deterministic)
analyseDeterministicWithCASS opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Deterministic" "cass-deterministic" parseDeterministic (CurryOperation pkg vsn m) jsOperationCASSDeterministic

-- This action initiates a call to CASS to compute the 'Demand' analysis for the given module in
-- the given path.
analyseDemandWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Demand)
analyseDemandWithCASS opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Demand" "cass-demand" parseDemand (CurryOperation pkg vsn m) jsOperationCASSDemand

-- This action initiates a call to CASS to compute the 'Indeterministic' analysis for the given module in
-- the given path.
analyseIndeterministicWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Indeterministic)
analyseIndeterministicWithCASS opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Indeterministic" "cass-indeterministic" parseIndeterministic (CurryOperation pkg vsn m) jsOperationCASSIndeterministic

-- This action initiates a call to CASS to compute the 'SolComplete' analysis for the given module in
-- the given path.
analyseSolCompleteWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe SolComplete)
analyseSolCompleteWithCASS opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "SolComplete" "cass-solcomplete" parseSolComplete (CurryOperation pkg vsn m) jsOperationCASSSolComplete

-- This action initiates a call to CASS to compute the 'Terminating' analysis for the given module in
-- the given path.
analyseTerminatingWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Terminating)
analyseTerminatingWithCASS opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Terminating" "cass-terminating" parseTerminating (CurryOperation pkg vsn m) jsOperationCASSTerminating

-- This action initiates a call to CASS to compute the 'Total' analysis for the given module in
-- the given path.
analyseTotalWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe Total)
analyseTotalWithCASS opts pkg vsn m o = do
    analyseWithCASS opts pkg vsn m o "Total" "cass-total" parseTotal (CurryOperation pkg vsn m) jsOperationCASSTotal

-- This action initiates a call to the non-fail verification tool to compute
-- the call types and non-fail conditions for the given module.
analyseFailFree :: Options -> Package -> Version -> Module -> Operation
                -> IO (Maybe String)
analyseFailFree opts pkg vsn m o = do
  analyseWithCASS opts pkg vsn m o "FailFree" "failfree" Just
                  (CurryOperation pkg vsn m) jsOperationFailFree
