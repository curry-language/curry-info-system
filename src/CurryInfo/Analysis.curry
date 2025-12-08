------------------------------------------------------------------------------
--- This modules defines operations to start analysis using external tools
--- like CASS or `verify-non-fail`.
------------------------------------------------------------------------------

module CurryInfo.Analysis where

import Data.Map    ( Map, fromList, toList )
import JSON.Data
import JSON.Parser ( parseJSON )
import JSON.Convert

import CurryInfo.Checkout
import CurryInfo.Commands
import CurryInfo.Helper       ( quote, safeRead )
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

-- Representation of qualified names.
type QName = (String,String)

--- Map a request name of CurryInfo to a CASS analysis name together with
--- operations to transform a list of qualified names and values as
--- Curry terms into a map representation (as used by CASS) and vice versa.
curryInfoRequest2CASS ::
  [(String, (String, CurryOutputTerm -> String, String -> CurryOutputTerm))]
curryInfoRequest2CASS =
  [ ("deterministic",     ("Deterministic",
                           toDetMap, fromDetMap "deterministic"))
  , ("demand",            ("Demand", toDemandMap, fromDemandMap "demand"))
  , ("indeterministic",   ("Indeterministic",
                           toBoolMap, fromBoolMap "indeterministic"))
  , ("solution-complete", ("SolComplete",
                           toBoolMap, fromBoolMap "solution-complete"))
  , ("terminating",       ("Terminating",
                           toBoolMap, fromBoolMap "terminating"))
  , ("totally-defined",   ("Total", toBoolMap, fromBoolMap "totally-defined"))
  , ("result-values",     ("Values", toATypeMap, fromATypeMap "result-values"))
  ]
 where
  toDetMap    ps = show (curryTerm2Map ps :: Map QName Deterministic)
  toDemandMap ps = show (curryTerm2Map ps :: Map QName [Int])
  toBoolMap   ps = show (curryTerm2Map ps :: Map QName Bool)
  toATypeMap  ps = show (curryTerm2Map ps :: Map QName AType)

  fromDetMap    rq ms = map2CurryTerm rq (read ms :: Map QName Deterministic)
  fromDemandMap rq ms = map2CurryTerm rq (read ms :: Map QName [Int])
  fromBoolMap   rq ms = map2CurryTerm rq (read ms :: Map QName Bool)
  fromATypeMap  rq ms = map2CurryTerm rq (read ms :: Map QName AType)

--- Map a `CurryOutputTerm` representing results for a list of operations
--- (i.e., where the first components contains qualified names) and
--- values for a _single_ request into a map from QNames to request values.
curryTerm2Map :: Read a => CurryOutputTerm -> Map QName a
curryTerm2Map qnvs =
  fromList (map (\(qn,v) -> (readQName qn, readSingleRequestValue v)) qnvs)
 where
  readSingleRequestValue :: Read a => [(String,String)] -> a
  readSingleRequestValue rvs = case rvs of
    [(_,s)] -> case reads s of
                 [(v, "")] -> v
                 _         -> error $ "curryTerm2Map: Read error on: " ++ s
    _       -> error $ "curryTerm2Map: no unique result value in:\n" ++ show rvs

  readQName s = case words s of
    [mn,en] -> (mn,en)
    _       -> error $ "curryTerm2Map: no qualified name found: " ++ s

--- Transform a map from QNames to request values into a corresponding
--- `CurryOutputTerm`. This is the inverse of operation `curryTerm2Map`
--- where the first argument is the request identifier.
map2CurryTerm :: Show a => String -> Map QName a -> CurryOutputTerm
map2CurryTerm req qnmap =
  map (\ ((mn,fn),aval) -> (mn ++ " " ++ fn, [(req,show aval)])) (toList qnmap)

-- Analyse an operation of a module with CASS where the name of the
-- CurryInfo field is provided as the last argument.
analyseOperationWithCASS :: Options -> Package -> Version -> Module -> Operation
                         -> String -> IO (Maybe String)
analyseOperationWithCASS opts pkg vsn mn o field =
  case lookup field curryInfoRequest2CASS of
    Nothing        -> do printErrorMessage $
                           "No CASS analysis found for field " ++
                           quote field ++ "!"
                         return Nothing
    Just (aname,_,_) -> analyseWith (cmdCASS opts) opts pkg vsn mn o aname field
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
