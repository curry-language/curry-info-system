-----------------------------------------------------------------------------------------
--- This modules defines operations to start analysis using outside tools like CASS.
-----------------------------------------------------------------------------------------

module CurryInfo.Analysis where

import CurryInfo.Types
import CurryInfo.Commands
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Paths
import CurryInfo.Checkout
import CurryInfo.Writer
import CurryInfo.Reader

import JSON.Data
import JSON.Parser (parseJSON)
import JSON.Convert

import XML

-- Analysis

-- This action initiates a call to CASS to compute the given analysis for the
-- given module.
analyseWithCASS :: Options -> Package -> Version -> Module -> String -> String
                -> String -> (String -> QueryObject) -> IO (Maybe String)
analyseWithCASS opts pkg vsn m name analysis field constructor = do
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
        case parseJSON output of
          Just (JArray jvs) -> do
            printDebugMessage opts "Looking for results..."
            case getJsonResults jvs of
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

                    return (Just result)
          _ -> do
            printDebugMessage opts "Could not parse output. Expected json."
            printDebugMessage opts "Output:"
            printDebugMessage opts output
            printDetailMessage opts "Analysis failed."
            return Nothing
  where
    getJsonResults :: [JValue] -> Maybe [(String, String)]
    getJsonResults = mapM elemToResult

    elemToResult :: JValue -> Maybe (String, String)
    elemToResult jv = case jv of
      JObject fields -> do
        n <- lookup "name" fields >>= fromJSON
        r <- lookup "result" fields >>= fromJSON
        return (n, r)
      _ -> Nothing

    addInformation :: (String, String) -> IO ()
    addInformation (n, r) = do
      let obj = constructor n
      initializeStore obj
      mfields <- readObjectInformation opts obj
      case mfields of
        Nothing -> do
          printDebugMessage opts $ errorReadingObject obj
        Just fields -> do
          let newInformation = [(field, toJSON r)] <+> fields
          writeObjectInformation obj newInformation

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis for the given module in
-- the given path.
analyseUnsafeModuleWithCASS :: Options -> Package -> Version -> Module -> IO (Maybe String)
analyseUnsafeModuleWithCASS opts pkg vsn m =
  analyseWithCASS opts pkg vsn m m "UnsafeModule" "cass-unsafemodule" (ModuleObject pkg vsn)

-- This action initiates a call to CASS to compute the 'Deterministic' analysis for the given module in
-- the given path.
analyseDeterministicWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe String)
analyseDeterministicWithCASS opts pkg vsn m o =
  analyseWithCASS opts pkg vsn m o "Deterministic" "cass-deterministic" (OperationObject pkg vsn m)

-- This action initiates a call to CASS to compute the 'Demand' analysis for the given module in
-- the given path.
analyseDemandWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe String)
analyseDemandWithCASS opts pkg vsn m o =
  analyseWithCASS opts pkg vsn m o "Demand" "cass-demand" (OperationObject pkg vsn m)

-- This action initiates a call to CASS to compute the 'Indeterministic' analysis for the given module in
-- the given path.
analyseIndeterministicWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe String)
analyseIndeterministicWithCASS opts pkg vsn m o =
  analyseWithCASS opts pkg vsn m o "Indeterministic" "cass-indeterministic" (OperationObject pkg vsn m)

-- This action initiates a call to CASS to compute the 'SolComplete' analysis for the given module in
-- the given path.
analyseSolCompleteWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe String)
analyseSolCompleteWithCASS opts pkg vsn m o =
  analyseWithCASS opts pkg vsn m o "SolComplete" "cass-solcomplete" (OperationObject pkg vsn m)

-- This action initiates a call to CASS to compute the 'Terminating' analysis for the given module in
-- the given path.
analyseTerminatingWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe String)
analyseTerminatingWithCASS opts pkg vsn m o =
  analyseWithCASS opts pkg vsn m o "Terminating" "cass-terminating" (OperationObject pkg vsn m)

-- This action initiates a call to CASS to compute the 'Total' analysis for the given module in
-- the given path.
analyseTotalWithCASS :: Options -> Package -> Version -> Module -> Operation -> IO (Maybe String)
analyseTotalWithCASS opts pkg vsn m o =
  analyseWithCASS opts pkg vsn m o "Total" "cass-total" (OperationObject pkg vsn m)

-- This action initiates a call to the non-fail verification tool to compute
-- the call types and non-fail conditions for the given module.
analyseFailFree :: Options -> Package -> Version -> Module -> Operation
        -> IO (Maybe String)
analyseFailFree opts pkg vsn m o =
  analyseWithCASS opts pkg vsn m o "FailFree" "failfree"
                 (OperationObject pkg vsn m)
 