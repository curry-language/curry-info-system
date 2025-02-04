------------------------------------------------------------------------------
--- This modules defines operations to start analysis using outside tools
--- like CASS or `verify-non-fail`.
------------------------------------------------------------------------------

module CurryInfo.Analysis where

import JSON.Data
import JSON.Parser (parseJSON)
import JSON.Convert

import XML

import CurryInfo.Checkout
import CurryInfo.Commands
import CurryInfo.Paths
import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage, printErrorMessage )
import CurryInfo.Writer
import CurryInfo.Reader

-- Analysis

-- This action invokes an analysis tool, like CASS, to perform
-- the given analysis on the given module.
-- The first argument is an operation (like `withCASS`) which returns,
-- for a given file path, analysis and module, the command to compute
-- the analysis information.
analyseWith :: (FilePath -> String -> Module
                    -> (String, IO (Int, String, String)))
                -> Options -> Package -> Version -> Module -> String -> String
                -> String -> (String -> QueryObject) -> IO (Maybe String)
analyseWith anacmd opts pkg vsn m ename analysis field constructor = do
  printDetailMessage opts $ "Starting analysis '" ++ analysis ++ "'..."
  mpath <- checkoutIfMissing opts pkg vsn
  case mpath of
    Nothing -> do
      printDetailMessage opts "Analysis failed."
      return Nothing
    Just path -> do
      (_, output, _) <- runCmd opts (anacmd path analysis m)
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
              if null ename -- dummy object?
                then return (Just "")
                else lookupResultForEntity results
        _ -> do
          printErrorMessage $
            "Could not parse JSON output of analysis '" ++ analysis ++
            "' of module '" ++ m ++ "'!"
          printErrorMessage "Analysis output:"
          printErrorMessage output
          printDetailMessage opts $ "Analysis'" ++ analysis ++ "' failed."
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
    _              -> Nothing

  addInformation :: (String, String) -> IO ()
  addInformation (n, r) = do
    let obj = constructor n
    updateObjectInformation opts obj [(field, toJSON r)]
  
  lookupResultForEntity results = do
    printDebugMessage opts "Results found. Looking for requested result..."
    case lookup ename results of
      Nothing -> do
        printDebugMessage opts $
         "Could not find entry with name '" ++ ename ++ "' in analysis results."
        printDetailMessage opts "Analysis failed."
        return Nothing
      Just result -> do
        printDetailMessage opts "Analysis succeeded."
        printDebugMessage opts $ "Result found: " ++ show result
        return (Just result)

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis
-- for the given module in the given path.
analyseUnsafeModuleWithCASS :: Options -> Package -> Version -> Module
                            -> IO (Maybe String)
analyseUnsafeModuleWithCASS opts pkg vsn m =
  analyseWith (cmdCASS opts) opts pkg vsn m m
              "UnsafeModule" "cass-unsafemodule" (QueryModule pkg vsn)

-- This action initiates a call to CASS to compute the 'Deterministic' analysis
-- for the given module in the given path.
analyseDeterministicWithCASS :: Options -> Package -> Version -> Module
                             -> Operation -> IO (Maybe String)
analyseDeterministicWithCASS opts pkg vsn m o =
  analyseWith (cmdCASS opts) opts pkg vsn m o
              "Deterministic" "cass-deterministic" (QueryOperation pkg vsn m)

-- This action initiates a call to CASS to compute the 'Demand' analysis
-- for the given module in the given path.
analyseDemandWithCASS :: Options -> Package -> Version -> Module
                      -> Operation -> IO (Maybe String)
analyseDemandWithCASS opts pkg vsn m o =
  analyseWith (cmdCASS opts) opts pkg vsn m o
              "Demand" "cass-demand" (QueryOperation pkg vsn m)

-- This action initiates a call to CASS to compute the 'Indeterministic'
-- analysis for the given module in the given path.
analyseIndeterministicWithCASS :: Options -> Package -> Version -> Module
                              -> Operation -> IO (Maybe String)
analyseIndeterministicWithCASS opts pkg vsn m o =
  analyseWith (cmdCASS opts) opts pkg vsn m o
    "Indeterministic" "cass-indeterministic" (QueryOperation pkg vsn m)

-- This action initiates a call to CASS to compute the 'SolComplete' analysis
-- for the given module in the given path.
analyseSolCompleteWithCASS :: Options -> Package -> Version -> Module
                           -> Operation -> IO (Maybe String)
analyseSolCompleteWithCASS opts pkg vsn m o =
  analyseWith (cmdCASS opts) opts pkg vsn m o
    "SolComplete" "cass-solcomplete" (QueryOperation pkg vsn m)

-- This action initiates a call to CASS to compute the 'Terminating' analysis
-- for the given module in the given path.
analyseTerminatingWithCASS :: Options -> Package -> Version -> Module
                           -> Operation -> IO (Maybe String)
analyseTerminatingWithCASS opts pkg vsn m o =
  analyseWith (cmdCASS opts) opts pkg vsn m o
    "Terminating" "cass-terminating" (QueryOperation pkg vsn m)

-- This action initiates a call to CASS to compute the 'Total' analysis
-- for the given module in the given path.
analyseTotalWithCASS :: Options -> Package -> Version -> Module
                     -> Operation -> IO (Maybe String)
analyseTotalWithCASS opts pkg vsn m o =
  analyseWith (cmdCASS opts) opts pkg vsn m o
    "Total" "cass-total" (QueryOperation pkg vsn m)

-- This action initiates a call to CASS to compute the 'Total' analysis
-- for the given module in the given path.
analyseValuesWithCASS :: Options -> Package -> Version -> Module
                     -> Operation -> IO (Maybe String)
analyseValuesWithCASS opts pkg vsn m o =
  analyseWith (cmdCASS opts) opts pkg vsn m o
    "Values" "cass-values" (QueryOperation pkg vsn m)

-- This action initiates a call to the non-fail verification tool to compute
-- the call types and non-fail conditions for the given module.
analyseFailFree :: Options -> Package -> Version -> Module -> Operation
                -> IO (Maybe String)
analyseFailFree opts pkg vsn m o =
  analyseWith (cmdCallTypes opts ["-v1"]) opts pkg vsn m o
              "FailFree" "failfree" (QueryOperation pkg vsn m)

-- This action initiates a call to the non-fail verification tool to compute
-- the in/out types for the given module.
analyseIOTypes :: Options -> Package -> Version -> Module -> Operation
               -> IO (Maybe String)
analyseIOTypes opts pkg vsn m o =
  analyseWith (cmdCallTypes opts ["-v0", "--iotypes"]) opts pkg vsn m o
               "IOType" "iotype" (QueryOperation pkg vsn m)
