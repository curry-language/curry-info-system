------------------------------------------------------------------------------
--- This modules defines operations to start analysis using external tools
--- like CASS or `verify-non-fail`.
------------------------------------------------------------------------------

module CurryInfo.Analysis where

import JSON.Data
import JSON.Parser (parseJSON)
import JSON.Convert

import CurryInfo.Checkout
import CurryInfo.Commands
import CurryInfo.Helper       ( quote )
import CurryInfo.Paths
import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Verbosity    ( printStatusMessage, printDetailMessage
                              , printDebugMessage, printErrorMessage )
import CurryInfo.Writer
import CurryInfo.Reader

------------------------------------------------------------------------------

-- This action invokes an analysis tool, like CASS, to perform
-- the given analysis on the given module and stores the computed
-- analysis results.
-- The first argument is an operation (like `withCASS`) which returns,
-- for a given file path, analysis name and module, the command to compute
-- the analysis information. It is assumed that this operation returns
-- a JSON array where each element is a JSON object containing the fields
-- `name` and `result` (the name of an entity and its analysis result).
-- The further arguments are the options, package, version, module,
-- entitiy name, analysis name, field name (containing the analysis result),
-- and a function which maps an entity name into a corresponding query object.
analyseWith :: (FilePath -> String -> Module
                                   -> (String, IO (Int, String, String)))
            -> Options -> Package -> Version -> Module -> String -> String
            -> String -> (String -> QueryObject) -> IO (Maybe String)
analyseWith anacmd opts pkg vsn mn ename ananame field constructor = do
  printDetailMessage opts $ "Starting analysis '" ++ ananame ++ "'..."
  mpath <- checkoutIfMissing opts pkg vsn
  case mpath of
    Nothing -> do
      printDetailMessage opts "Analysis failed."
      return Nothing
    Just path -> do
      (_, output, _) <- runCmd opts (anacmd path ananame mn)
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
              mapM_ addInformation results
              if null ename -- dummy object?
                then return (Just "")
                else lookupResultForEntity results
        _ -> do
          printErrorMessage $
            "Could not parse JSON output of analysis '" ++ ananame ++
            "' of module '" ++ mn ++ "'!"
          printErrorMessage "Analysis output:"
          printErrorMessage output
          printDetailMessage opts $ "Analysis'" ++ ananame ++ "' failed."
          return Nothing
 where
  getJsonResults :: [JValue] -> Maybe [(String, String)]
  getJsonResults = mapM elemToResult
   where
    elemToResult jv = case jv of
      JObject fields -> do
        n <- lookupName "name"   fields >>= fromJSON
        r <- lookupName "result" fields >>= fromJSON
        return (n, r)
      _              -> Nothing

  addInformation :: (String, String) -> IO ()
  addInformation (n, r) =
    updateObjectInformation opts (constructor n) [(field, toJSON r)]
  
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

-- Map a request name of CurryInfo to a CASS analysis name.
curryInfoRequest2CASS :: [(String,String)]
curryInfoRequest2CASS =
  [ ("deterministic",     "Deterministic")
  , ("demand",            "Demand")
  , ("indeterministic",   "Indeterministic")
  , ("solution-complete", "SolComplete")
  , ("terminating",       "Terminating")
  , ("totally-defined",   "Total")
  , ("result-values",     "Values")
  ]

-- Analyse an operation of a module with CASS where the name of the
-- CurryInfo field is provided as the last argument.
analyseOperationWithCASS :: Options -> Package -> Version -> Module -> Operation
                         -> String -> IO (Maybe String)
analyseOperationWithCASS opts pkg vsn mn o field =
  case lookup field curryInfoRequest2CASS of
    Nothing    -> do printErrorMessage $ "No CASS analysis found for field " ++
                                         quote field ++ "!"
                     return Nothing
    Just aname -> analyseWith (cmdCASS opts) opts pkg vsn mn o aname field
                              (QueryOperation pkg vsn mn)

-- This action initiates a call to the non-fail verification tool to compute
-- the call types and non-fail conditions for the given module.
analyseFailFree :: Options -> Package -> Version -> Module -> Operation
                -> IO (Maybe String)
analyseFailFree opts pkg vsn mn o =
  analyseWith (cmdCallTypes opts ["-v1"]) opts pkg vsn mn o
              "FailFree" "failfree" (QueryOperation pkg vsn mn)

-- This action initiates a call to the non-fail verification tool to compute
-- the in/out types for the given module.
analyseIOTypes :: Options -> Package -> Version -> Module -> Operation
               -> IO (Maybe String)
analyseIOTypes opts pkg vsn mn o =
  analyseWith (cmdCallTypes opts ["-v0", "--iotypes"]) opts pkg vsn mn o
               "IOType" "iotype" (QueryOperation pkg vsn mn)

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis
-- for the given module in the given path.
analyseUnsafeModuleWithCASS :: Options -> Package -> Version -> Module
                            -> IO (Maybe String)
analyseUnsafeModuleWithCASS opts pkg vsn mn =
  analyseWith (cmdCASS opts) opts pkg vsn mn mn
              "UnsafeModule" "unsafe" (QueryModule pkg vsn)
