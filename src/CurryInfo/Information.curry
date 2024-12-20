-----------------------------------------------------------------------------
--- This modules defines operations to create outputs and process requests
--- to get information about objects.
-----------------------------------------------------------------------------

module CurryInfo.Information ( getInfos, printResult ) where

import CurryInfo.Configuration
import CurryInfo.Paths     ( index, getDirectoryPath, getJSONPath
                           , initializeStore, jsonFile2Name )
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.Options   ( queryOptions )
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage)
import CurryInfo.Generator ( readPackageJSON, getExportedModules
                           , readPackageModules)
import CurryInfo.Helper    ( InformationResult(..), information, quote )

import JSON.Pretty (ppJSON)
import JSON.Data

import Data.Char   ( toLower )
import Data.Either ( partitionEithers )
import Data.List   ( isSuffixOf, union )
import Data.Maybe  ( catMaybes, isJust )

import System.Environment ( getArgs )
import System.FilePath    ( (</>), (<.>) )
import System.Directory   ( doesDirectoryExist, doesFileExist
                          , getDirectoryContents )

import Control.Monad (unless, zipWithM, when)

--- This action prints the given output to stdout and also returns
--- the string as result.
printResult :: Options -> Output -> IO String
printResult opts (OutputText txt)  = printAndReturn opts txt
printResult opts (OutputJSON jv)   = printAndReturn opts (ppJSON jv)
printResult opts (OutputTerm ts)   = printAndReturn opts (show ts)
printResult opts (OutputError err) = printAndReturn opts ("Error: " ++ err)

printAndReturn :: Options -> String -> IO String
printAndReturn opts s =
  let ofile = optOutFile opts
  in (if null ofile then putStrLn else writeFile ofile) s >> return s

--- This action returns a failed output with the given error message.
generateOutputError :: Options -> String -> IO Output
generateOutputError opts err = do
  printDetailMessage opts err
  return $ case optOutFormat opts of OutText -> OutputText err
                                     OutJSON -> OutputJSON (JString err)
                                     OutTerm -> OutputTerm []

--- This action process the given requests for the given query object and
--- returns the output for the requests.
getInfos :: Options -> QueryObject -> [String] -> IO Output
getInfos opts qobj reqs = do
  printDetailMessage opts $
    "Checking structure of request " ++ quotePrettyObject qobj
  case qobj of
    QueryPackage pkg -> do
      printDetailMessage opts "Request is Package."
      result <- checkPackageExists pkg
      case result of
        False ->
          printDebugAndOutputError $ "Package '" ++ pkg ++ "' does not exist."
        True -> do
          printDebugMessage opts "Package exists."
          getInfosConfig opts qobj reqs packageConfiguration (CurryPackage pkg)
    QueryVersion pkg vsn -> do
      printDetailMessage opts "Request is Version."
      result <- checkVersionExists pkg vsn
      case result of
        False ->
          printDebugAndOutputError $ "Version '" ++ vsn ++ "' of package '" ++
            pkg ++ "' does not exist."
        True -> do
          printDetailMessage opts "Version entity exists."
          getInfosConfig opts qobj reqs
                         versionConfiguration (CurryVersion pkg vsn)
    QueryModule pkg vsn m -> do
      printDetailMessage opts "Request is Module."
      result <- checkModuleExists pkg vsn m
      case result of
        False -> do
          printDebugAndOutputError $ "Module '" ++ m ++ "' of version '" ++
            vsn ++ "' of package '" ++ pkg ++ "' is not exported."
        True  -> do
          printDetailMessage opts "Module entity exists."
          case (optAllTypes opts, optAllClasses opts, optAllOperations opts) of
            (True, _, _) -> do
              mts <- queryAllEntities pkg vsn m (QueryType pkg vsn m "?")
                                      "types"
              case mts of
                Nothing -> do
                  generateOutputError opts "Could not find types"
                Just ts -> do
                  outs <- mapM (\(qo,o) -> getInfosConfig opts qo reqs
                                             typeConfiguration o)
                               (map (\t -> (QueryType pkg vsn m t,
                                            CurryType pkg vsn m t)) ts)
                  let out = combineOutput outs
                  return out
            (_, True, _) -> do
              mcs <- queryAllEntities pkg vsn m (QueryClass pkg vsn m "?")
                                      "classes"
              case mcs of
                Nothing -> do
                  generateOutputError opts "Could not find type classes"
                Just cs -> do
                  outs <- mapM (\(qo,o) -> getInfosConfig opts qo reqs
                                             classConfiguration o)
                               (map (\t -> (QueryClass pkg vsn m t,
                                            CurryClass pkg vsn m t)) cs)
                  let out = combineOutput outs
                  return out
            (_, _, True) -> do
              mos <- queryAllEntities pkg vsn m (QueryOperation pkg vsn m "?")
                                      "operations"
              case mos of
                Nothing ->
                  generateOutputError opts "Could not find operations."
                Just os -> do
                  outs <- mapM (\(qo,o) -> getInfosConfig opts qo reqs
                                             operationConfiguration o)
                               (map (\op -> (QueryOperation pkg vsn m op,
                                             CurryOperation pkg vsn m op)) os)
                  let out = combineOutput outs
                  return out
            (False, False, False) -> do
              getInfosConfig opts qobj reqs
                             moduleConfiguration (CurryModule pkg vsn m)
    QueryType pkg vsn m t -> do
      printDetailMessage opts "Request is Type"
      result <- checkEntityExists pkg vsn m t (QueryType pkg vsn m t) "types"
      case result of
        False -> do
          printDebugAndOutputError $ noExistMessage pkg vsn m "Type" t
        True  -> do
          printDetailMessage opts $
            "Type " ++ quotePrettyObject qobj ++ " exists."
          getInfosConfig opts qobj reqs
                         typeConfiguration (CurryType pkg vsn m t)
    QueryClass pkg vsn m c -> do
      printDetailMessage opts "Request is Class"
      result <- checkEntityExists pkg vsn m c (QueryClass pkg vsn m c) "classes"
      case result of
        False -> 
          printDebugAndOutputError $ noExistMessage pkg vsn m "Class" c
        True  -> do
          printDetailMessage opts $
            "Class " ++ quotePrettyObject qobj ++ " exists."
          getInfosConfig opts qobj reqs
                         classConfiguration (CurryClass pkg vsn m c)
    QueryOperation pkg vsn m o -> do
      printDetailMessage opts "Request is Operation."
      result <- checkEntityExists pkg vsn m o (QueryOperation pkg vsn m o)
                                  "operations"
      case result of
        False ->
          printDebugAndOutputError $ noExistMessage pkg vsn m "Operation" o
        True  -> do
          printDetailMessage opts $
            "Operation " ++ quotePrettyObject qobj ++ " exists."
          getInfosConfig opts qobj reqs
                         operationConfiguration (CurryOperation pkg vsn m o)
 where
  noExistMessage pkg vsn m ek e = unwords
    [ ek, quote e, "of module", quote m, "of version", quote vsn, "of package"
    , quote pkg, "is not exported."]
  
  printDebugAndOutputError err = do printDetailMessage opts err
                                    generateOutputError opts err

  -- Return all entities of the query object currently stored in json files:
  queryAllStoredEntities :: QueryObject -> IO [String]
  queryAllStoredEntities qo = do
    dir   <- getDirectoryPath qo
    exdir <- doesDirectoryExist dir
    if exdir then do jsonfiles <- fmap (catMaybes . map jsonFile2Name)
                                       (getDirectoryContents dir)
                     return jsonfiles
             else return []

  -- Return all entities of the query object.
  queryAllEntities pkg vsn m entqobj entreq = do
    stnames <- queryAllStoredEntities entqobj
    opnames <- query (QueryModule pkg vsn m) entreq
    return (opnames >>= Just . union stnames)

  query :: Read a => QueryObject -> String -> IO (Maybe a)
  query obj req = do
    printDetailMessage opts $ "Query for object " ++ quotePrettyObject obj ++
                              " and request " ++ quote req
    res <- getInfos queryOptions obj [req]
    printDebugMessage opts $ "Query result: " ++ show res
    case res of
      -- OutputTerm [("obj", [("req", "res")])]
      OutputTerm [(_, x)] -> case lookup req (read x) of
        Nothing -> return Nothing
        Just y  -> return (Just (read y))
      _                   -> return Nothing

  combineOutput :: [Output] -> Output
  combineOutput outs = case optOutFormat opts of
    OutText -> OutputText (unlines (map fromOutputText outs))
    OutJSON -> OutputJSON (JArray (map fromOutputJSON outs))
    OutTerm -> OutputTerm (concatMap fromOutputTerm outs)
  
  fromOutputText :: Output -> String
  fromOutputText out = case out of OutputText txt -> txt
                                   _              -> error "fromOutputText"

  fromOutputJSON :: Output -> JValue
  fromOutputJSON out = case out of OutputJSON jv -> jv
                                   _             -> error "fromOutputJSON"

  fromOutputTerm :: Output -> [(String, String)]
  fromOutputTerm out = case out of OutputTerm ts -> ts
                                   _             -> error "fromOutputTerm"

  checkPackageExists :: Package -> IO Bool
  checkPackageExists pkg = do
    jpath <- getJSONPath (QueryPackage pkg)
    whenFileDoesNotExist jpath $do
      path <- index
      printDebugMessage opts $ "Looking for package in index..."
      doesDirectoryExist (path </> pkg)

  checkVersionExists :: Package -> Version -> IO Bool
  checkVersionExists pkg vsn = do
    jpath <- getJSONPath (QueryVersion pkg vsn)
    whenFileDoesNotExist jpath $ do
      path <- index
      printDebugMessage opts $ "Looking for version in index..."
      doesDirectoryExist (path </> pkg </> vsn)

  checkModuleExists :: Package -> Version -> Module -> IO Bool
  checkModuleExists pkg vsn m = do
    jpath <- getJSONPath (QueryModule pkg vsn m)
    whenFileDoesNotExist jpath $ do
      allMods <- readPackageModules opts pkg vsn
      mbjson  <- readPackageJSON opts pkg vsn
      let exportedmods = maybe allMods id
                               (mbjson >>= Just  . snd >>= getExportedModules)
      return (elem m exportedmods)

  checkEntityExists pkg vsn m e qentity ereq = do
    jpath <- getJSONPath qentity
    whenFileDoesNotExist jpath $ do
      res <- query (QueryModule pkg vsn m) ereq
      case res of Nothing -> return False
                  Just es -> return $ elem e es
    
whenFileDoesNotExist :: FilePath -> IO Bool -> IO Bool
whenFileDoesNotExist path act = do
  exf <- doesFileExist path
  case exf of True  -> return True
              False -> act

--- This action process the given requests for the given query object
--- w.r.t. to the configuration for the kind of query object and
--- returns the output for the requests.
getInfosConfig :: Show a => Options -> QueryObject -> [String]
               -> [RegisteredRequest a] -> a -> IO Output
getInfosConfig opts queryobject reqs conf configobject = do
  printDetailMessage opts $
    "Initializing store for entity " ++ quotePrettyObject queryobject
  initializeStore queryobject
  printDetailMessage opts $
    "Reading current information of entity " ++ quotePrettyObject queryobject
  mfields <- readObjectInformation opts queryobject
  case mfields of
    Nothing -> do
      printDetailMessage opts "Reading information failed."
      return $ OutputError $ errorReadingObject queryobject
    Just fields -> do
      printDetailMessage opts "Reading information succeeded."
      case optShowAll opts of
        True -> do
          printDetailMessage opts
            "Returning all currently available information..."
          let fieldNames = map fst fields
          case mapM (flip lookupRequest conf) fieldNames of
            Nothing -> do
              return $ OutputError $
                "One of the fields could not be found: " ++ show fieldNames
            Just allReqs -> do
              results <- zipWithM
                           (\(_, _, extractor, _) fieldName ->
                              fmap (\x -> (fieldName, x))
                                   (extract opts extractor fields))
                           allReqs
                           fieldNames :: IO [(String, Maybe (JValue, String))]
              let results' = map (\(r, mr) ->
                                  (r,
                                   maybe InformationExtractionFailed
                                         (uncurry InformationResult) mr))
                                 results
              let output = createOutput queryobject results' :: Output
              return output
        False -> do
          printDetailMessage opts
            "Extracting/Generating requested information..."
          results <- mapM (extractOrGenerate fields configobject
                              (errorRequestObject queryobject))
                          (map (map toLower) reqs)
                            :: IO [(String, InformationResult)]

          let newInformation = createNewInformation results
          printDebugMessage opts "Overwriting with updated information..."
          writeObjectInformation queryobject (newInformation <+> fields)
          printDetailMessage opts "Overwriting finished."

          let output = createOutput queryobject results :: Output
          return output
 where  
  createNewInformation :: [(String, InformationResult)] -> [(String, JValue)]
  createNewInformation =
    foldr (\(r, ir) acc ->
              information acc (const acc) (\jv _ -> (r, jv):acc) ir) []

  -- generate output for a single entity?
  outputSingleEntity = not
    (optAllTypes opts || optAllClasses opts || optAllOperations opts)
  
  createOutput :: QueryObject -> [(String, InformationResult)] -> Output
  createOutput obj results = case optOutFormat opts of
    OutText -> OutputText $ unlines $
                 (if outputSingleEntity then []
                                        else [object2StringTuple obj]) ++
                 map (\(r, ir) -> r ++ ": " ++ 
                        information "?" id (flip const) ir)
                    results
    OutJSON -> OutputJSON $ JObject
                  [("object", (JString . object2StringTuple) obj),
                   ("results",
                    JObject (map (\(r, ir) ->
                                    (r, information JNull JString
                                          (\_ s -> JString s) ir))
                                results))]
    OutTerm -> OutputTerm
                  [(object2StringTuple obj,
                    show (map (\(r, ir) ->
                                  (r, information "?" id (flip const) ir))
                              results))]

  extractOrGenerate fields obj reqerrormsg req = do
    printDetailMessage opts $
      "\nProcessing request '" ++ req ++ "' for " ++ show obj ++ "..."
    case lookupRequest req conf of
      Nothing -> do
        let msg = reqerrormsg req
        printDetailMessage opts $ msg
        return (req, InformationError "REQUEST DOES NOT EXIST IN CONFIGURATION")
      Just (_, _, extractor, generator) -> do
        printDetailMessage opts
          "Request found in configuration. Looking at Force option..."
        case optForce opts of
          0 -> do
            printDebugMessage opts "Force option 0: Only extraction"
            extractionResult <- extract opts extractor fields
            case extractionResult of
              Nothing -> do
                printDetailMessage opts "Executing request failed"
                return (req, InformationExtractionFailed)
              Just (jv, output) -> do
                printDetailMessage opts "Executing request succeeded."
                return (req, InformationResult jv output)
          1 -> do
            printDebugMessage opts
              "Force option 1: Extraction, then generation if failed"
            extractionResult <- extract opts extractor fields
            case extractionResult of
              Nothing -> do
                generationResult <- generate opts generator obj
                case generationResult of
                  Nothing -> do
                    printDetailMessage opts "Executing request failed."
                    return (req,
                            InformationError "EXTRACTING AND GENERATING FAILED")
                  Just (jv, output) -> do
                    printDetailMessage opts "Executing request succeeded."
                    return (req, InformationResult jv output)
              Just (jv, output) -> do
                printDetailMessage opts "Executing request succeeded."
                return (req, InformationResult jv output)
          2 -> do
            printDebugMessage opts "Force option 2: Only generation"
            generationResult <- generate opts generator obj
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

extract :: Options -> (Options -> [(String, JValue)]
        -> IO (Maybe (JValue, String)))
        -> [(String, JValue)] -> IO (Maybe (JValue, String))
extract opts extractor fields = do
  printDebugMessage opts "Trying extraction..."
  extractionResult <- extractor opts fields
  case extractionResult of
    Nothing -> do
      printDetailMessage opts "Extraction failed: no such request in entity"
      return Nothing
    Just (jv, output) -> do
      printDetailMessage opts "Extraction succeeded."
      return $ Just (jv, output)
  
generate :: Options -> (Options -> a -> IO (Maybe (JValue, String))) -> a
         -> IO (Maybe (JValue, String))
generate opts generator obj = do
  printDebugMessage opts "Trying generation..."
  generationResult <- generator opts obj
  case generationResult of
    Nothing -> do
      printDetailMessage opts "Generation failed."
      return Nothing
    Just (jv, output) -> do
      printDetailMessage opts "Generation succeeded."
      return $ Just (jv, output)
    
