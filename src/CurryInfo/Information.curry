-----------------------------------------------------------------------------
--- This modules defines operations to create outputs and process requests
--- to get information about objects.
-----------------------------------------------------------------------------

module CurryInfo.Information ( getInfos, printResult ) where

import CurryInfo.Configuration
import CurryInfo.Paths     ( index, getJSONPath, initializeStore )
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.Options   ( queryOptions )
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage)
import CurryInfo.Generator ( readPackageJSON, getExportedModules
                           , readPackageModules)
import CurryInfo.Helper    ( InformationResult(..), information )

import JSON.Pretty (ppJSON)
import JSON.Data
import JSON.Parser (parseJSON)

import Data.Maybe (isJust)
import Data.Either (partitionEithers)
import Data.Char (toLower)

import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Directory (doesDirectoryExist, doesFileExist)

import Control.Monad (unless, zipWithM, when)

--- This action prints the given output to stdout and also returns
--- the string as result.
printResult :: Output -> IO String
printResult (OutputText txt)  = printAndReturn txt
printResult (OutputJSON jv)   = printAndReturn (ppJSON jv)
printResult (OutputTerm ts)   = printAndReturn (show ts)
printResult (OutputError err) = printAndReturn ("Error: " ++ err)

printAndReturn :: String -> IO String
printAndReturn s = putStrLn s >> return s

--- This action returns a failed output with the given error message.
generateOutputError :: Options -> String -> IO Output
generateOutputError opts err = do
  printDetailMessage opts err
  return $ case optOutput opts of OutText -> OutputText err
                                  OutJSON -> OutputJSON (JString err)
                                  OutTerm -> OutputTerm []

--- This action process the given requests for the given query object and
--- returns the output for the requests.
getInfos :: Options -> QueryObject -> [String] -> IO Output
getInfos opts qobj reqs = do
  printStatusMessage opts "Checking structure of the request..."
  case qobj of
    QueryPackage pkg -> do
      printStatusMessage opts "Structure matches Package."
      result <- checkPackageExists pkg
      case result of
        False -> do
          let err = "Package '" ++ pkg ++ "' does not exist."
          printDebugMessage opts err
          generateOutputError opts err
        True -> do
          printDebugMessage opts "Package exists."
          getInfosConfig opts qobj reqs packageConfiguration (CurryPackage pkg)
    QueryVersion pkg vsn -> do
      printStatusMessage opts "Structure matches Version."
      result <- checkVersionExists pkg vsn
      case result of
        False -> do
          let err = "Version '" ++ vsn ++ "' of package '" ++ pkg ++
                    "' does not exist."
          printDetailMessage opts err
          generateOutputError opts err
        True -> do
          printDetailMessage opts "Checked Version: exists."
          getInfosConfig opts qobj reqs
                         versionConfiguration (CurryVersion pkg vsn)
    QueryModule pkg vsn m -> do
      printStatusMessage opts "Structure matches Module."
      result <- checkModuleExists pkg vsn m
      case result of
        False -> do
          let err = "Module '" ++ m ++ "' of version '" ++ vsn ++
                    "' of package '" ++ pkg ++ "' is not exported."
          printDetailMessage opts err
          generateOutputError opts err
        True  -> do
          printDetailMessage opts "Checked Module: exists."
          case (optAllTypes opts, optAllTypeclasses opts, optAllOperations opts) of
            (True, _, _) -> do
              mts <- queryAllTypes pkg vsn m
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
              mcs <- queryAllTypeclasses pkg vsn m
              case mcs of
                Nothing -> do
                  generateOutputError opts "Could not find typeclasses"
                Just cs -> do
                  outs <- mapM (\(qo,o) -> getInfosConfig opts qo reqs
                                             typeclassConfiguration o)
                               (map (\t -> (QueryTypeClass pkg vsn m t,
                                            CurryTypeclass pkg vsn m t)) cs)
                  let out = combineOutput outs
                  return out
            (_, _, True) -> do
              mos <- queryAllOperations pkg vsn m
              case mos of
                Nothing -> do
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
      printStatusMessage opts "Structure matches Type"
      result <- checkTypeExists pkg vsn m t
      case result of
        False -> do
          let err = "Type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++
                    vsn ++ "' of package '" ++ pkg ++ "' is not exported."
          printDetailMessage opts err
          generateOutputError opts err
        True  -> do
          printDetailMessage opts "Checked Type: exists."
          getInfosConfig opts qobj reqs
                         typeConfiguration (CurryType pkg vsn m t)
    QueryTypeClass pkg vsn m c -> do
      printStatusMessage opts "Structure matches Typeclass"
      result <- checkTypeclassExists pkg vsn m c
      case result of
        False -> do
          let err = "Typeclass '" ++ c ++ "' of module '" ++ m ++
                    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++
                    "' is not exported."
          printDetailMessage opts err
          generateOutputError opts err
        True  -> do
          printDetailMessage opts "Checked Typeclass: exists."
          getInfosConfig opts qobj reqs
                         typeclassConfiguration (CurryTypeclass pkg vsn m c)
    QueryOperation pkg vsn m o -> do
      printStatusMessage opts "Structure matches Operation."
      result <- checkOperationExists pkg vsn m o
      case result of
        False -> do
          let err = "Operation '" ++ o ++ "' of module '" ++ m ++
                    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++
                    "' is not exported."
          printDetailMessage opts err
          generateOutputError opts err
        True  -> do
          printDetailMessage opts "Checked Operation: exists."
          getInfosConfig opts qobj reqs
                         operationConfiguration (CurryOperation pkg vsn m o)
 where
  queryAllTypes :: Package -> Version -> Module -> IO (Maybe [Type])
  queryAllTypes pkg vsn m = query (QueryModule pkg vsn m) "types"

  queryAllTypeclasses :: Package -> Version -> Module -> IO (Maybe [Typeclass])
  queryAllTypeclasses pkg vsn m = query (QueryModule pkg vsn m) "typeclasses"

  queryAllOperations :: Package -> Version -> Module -> IO (Maybe [Operation])
  queryAllOperations pkg vsn m = query (QueryModule pkg vsn m) "operations"

  query :: Read a => QueryObject -> String -> IO (Maybe a)
  query obj req = do
    res <- getInfos queryOptions obj [req] :: IO Output
    case res of
      -- OutputTerm [("obj", [("req", "res")])]
      OutputTerm [(_, x)] -> case lookup req (read x) of
        Nothing -> return Nothing
        Just y  -> return (Just (read y))
      _                   -> return Nothing

  combineOutput :: [Output] -> Output
  combineOutput outs = case optOutput opts of
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
      packageJSON <- readPackageJSON opts pkg vsn
      let exportedMods = maybe allMods id
                               (parseJSON packageJSON >>= getExportedModules)
      return (elem m exportedMods)

  checkTypeExists :: Package -> Version -> Module -> Type -> IO Bool
  checkTypeExists pkg vsn m t = do
    jpath <- getJSONPath (QueryType pkg vsn m t)
    whenFileDoesNotExist jpath $ do
      res <- query (QueryModule pkg vsn m) "types"
      case res of Nothing -> return False
                  Just ts -> return $ elem t ts

  checkTypeclassExists :: Package -> Version -> Module -> Typeclass -> IO Bool
  checkTypeclassExists pkg vsn m c = do
    jpath <- getJSONPath (QueryTypeClass pkg vsn m c)
    whenFileDoesNotExist jpath $ do
      res <- query (QueryModule pkg vsn m) "typeclasses"
      case res of Nothing -> return False
                  Just cs -> return $ elem c cs

  checkOperationExists :: Package -> Version -> Module -> Operation -> IO Bool
  checkOperationExists pkg vsn m o = do
    jpath <- getJSONPath (QueryOperation pkg vsn m o)
    whenFileDoesNotExist jpath $ do
      res <- query (QueryModule pkg vsn m) "operations"
      case res of Nothing -> return False
                  Just os -> return $ elem o os
    
whenFileDoesNotExist :: FilePath -> IO Bool -> IO Bool
whenFileDoesNotExist path act = do
  exf <- doesFileExist path
  case exf of True  -> return True
              False -> act

--- This action process the given requests for the given query object
--- w.r.t. to the configuration for the kind of query object and
--- returns the output for the requests.
getInfosConfig :: Options -> QueryObject -> [String]
              -> [RegisteredRequest a] -> a -> IO Output
getInfosConfig opts queryobject reqs conf configobject = do
  printStatusMessage opts "Initializing Input..."
  initializeStore queryobject
  printStatusMessage opts "Reading current information..."
  mfields <- readObjectInformation opts queryobject
  case mfields of
    Nothing -> do
      printDetailMessage opts "Reading information failed."
      return $ OutputError $ errorReadingObject queryobject
    Just fields -> do
      printDetailMessage opts "Reading information succeeded."
      case optShowAll opts of
        True -> do
          printStatusMessage opts
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
    (optAllTypes opts || optAllTypeclasses opts || optAllOperations opts)
  
  createOutput :: QueryObject -> [(String, InformationResult)] -> Output
  createOutput obj results = case optOutput opts of
    OutText -> OutputText $ unlines $
            (if outputSingleEntity then [] else [object2StringTuple obj]) ++
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
    printStatusMessage opts $ "\nProcessing request '" ++ req ++ "'..."
    case lookupRequest req conf of
      Nothing -> do
        let msg = reqerrormsg req
        printDetailMessage opts $ msg
        return (req, InformationError "REQUEST NOT FOUND")
      Just (_, _, extractor, generator) -> do
        printDetailMessage opts "Found request. Looking at Force option..."
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
      printDetailMessage opts "Extraction failed."
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
    
