-----------------------------------------------------------------------------
--- This modules defines operations to create outputs and process requests
--- to get information about objects.
-----------------------------------------------------------------------------

module CurryInfo.Information
  ( getAllPackageNames, getInfos, printResult )
 where

import Control.Monad      ( unless, when, zipWithM)
import Data.Char          ( toLower )
import Data.Either        ( partitionEithers )
import Data.List          ( (\\), find, isSuffixOf, partition, sort, union )
import Data.Maybe         ( catMaybes, isJust, isNothing )
import System.Environment ( getArgs )

import JSON.Convert        ( fromJSON )
import JSON.Data
import JSON.Pretty         ( ppJSON )
import System.Console.ANSI.Codes
import System.Directory    ( doesDirectoryExist, doesFileExist
                           , getDirectoryContents )
import System.FilePath     ( (</>), (<.>) )


import CurryInfo.Configuration
import CurryInfo.Paths     ( getCPMIndex, getReducedDirectoryContents
                           , jsonFile2Name, objectDirectory, objectJSONPath
                           , packagesPath, realNameField )
import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Reader
import CurryInfo.Writer
import CurryInfo.Options   ( getQueryOptions, withColor )
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage, printErrorMessage )
import CurryInfo.Generator ( readPackageJSON, getExportedModules
                           , readPackageModules)
import CurryInfo.Helper    ( RequestResult(..), fromQName
                           , quote, fromRequestResult )


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
--- In case of a Curry `OutTerm`, we generate an error message so that
--- a possible clients gets an error when parsing the output.
generateOutputError :: Options -> String -> IO Output
generateOutputError opts err = do
  printDetailMessage opts err
  return $ case optOutFormat opts of OutText -> OutputText err
                                     OutJSON -> OutputJSON (JString err)
                                     OutTerm -> OutputError err

--- Get the names of all packages stored in CurryInfo.
getAllPackageNames :: Options -> IO Output
getAllPackageNames opts = do
  let pkgdir = packagesPath opts
  printDetailMessage opts $ "Reading content of directory " ++ quote pkgdir
  contents <- sort <$> getReducedDirectoryContents pkgdir
  printDebugMessage opts $ "Packages found: " ++ unwords contents
  return $ case optOutFormat opts of
    OutText -> OutputText $
                withColor opts green "packages" ++ ": " ++ 
                unwords contents
    OutJSON -> OutputJSON $ JObject $ toJObject $
                  [("packages", JArray (map JString contents))]
    OutTerm -> OutputTerm [("packages", show contents)]

------------------------------------------------------------------------------
--- This action process the given requests for the given query object and
--- returns the output for the requests.
getInfos :: Options -> QueryObject -> [String] -> IO Output
getInfos opts qobj reqs = do
  printDetailMessage opts $
    "Checking structure of request " ++ quotePrettyObject qobj
  case qobj of
    QueryPackage pkg -> do
      printDetailMessage opts "Request is for Package entity."
      result <- checkPackageExists pkg
      case result of
        False ->
          printDebugAndOutputError $ "Package '" ++ pkg ++ "' does not exist."
        True -> do
          printDebugMessage opts "Package exists."
          getInfosConfig opts qobj reqs packageConfiguration (CurryPackage pkg)
    QueryVersion pkg vsn -> do
      printDetailMessage opts "Request is for Version entity."
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
      printDetailMessage opts "Request is for Module entity."
      result <- checkModuleExists pkg vsn m
      case result of
        False ->
          printDebugAndOutputError $ "Module '" ++ m ++ "' of version '" ++
            vsn ++ "' of package '" ++ pkg ++ "' is not exported."
        True  -> do
          printDetailMessage opts "Module entity exists."
          case (optAllTypes opts, optAllClasses opts, optAllOperations opts) of
            (True, _, _) -> checkRequests opts reqs typeConfiguration $ do
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
                  return $ combineOutput outs
            (_, True, _) -> checkRequests opts reqs classConfiguration $ do
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
                  return $ combineOutput outs
            (_, _, True) -> checkRequests opts reqs operationConfiguration $ do
              mos <- queryAllEntities pkg vsn m (QueryOperation pkg vsn m "?")
                                      "operations"
              case mos of
                Nothing ->
                  generateOutputError opts "Could not find operations."
                Just [] -> do
                  printDetailMessage opts "No operations found in module."
                  outs <- mapM (checkDummyAndGetInfosConfig
                                  operationConfiguration
                                  (QueryOperation pkg vsn m "")
                                  (CurryOperation pkg vsn m "")) reqs
                  return $ combineOutput outs
                Just os -> do
                  outs <- mapM (\(qo,o) -> getInfosConfig opts qo reqs
                                             operationConfiguration o)
                               (map (\op -> (QueryOperation pkg vsn m op,
                                             CurryOperation pkg vsn m op)) os)
                  return $ combineOutput outs
            (False, False, False) -> do
              getInfosConfig opts qobj reqs
                             moduleConfiguration (CurryModule pkg vsn m)
    QueryType pkg vsn m t -> do
      printDetailMessage opts "Request is for Type entity."
      ensureEntityExists pkg vsn m t qobj "types" "Type" $
        getInfosConfig opts qobj reqs typeConfiguration
                       (CurryType pkg vsn m t)
    QueryClass pkg vsn m c -> do
      printDetailMessage opts "Request is for Class entity."
      ensureEntityExists pkg vsn m c qobj "classes" "Class" $
        getInfosConfig opts qobj reqs classConfiguration
                       (CurryClass pkg vsn m c)
    QueryOperation pkg vsn m o -> do
      printDetailMessage opts "Request is for Operation entity."
      ensureEntityExists pkg vsn m o qobj "operations" "Operation" $
        getInfosConfig opts qobj reqs operationConfiguration
                       (CurryOperation pkg vsn m o)
 where
  -- Check whether the _DUMMY_ entity contains the request. If not,
  -- compute the requested information with `getInfosConfig`.
  -- This is necessary since a module might not contain explicitly
  -- exported operations but have operations which are generated and exported
  -- (e.g., operations for class instances).
  checkDummyAndGetInfosConfig entityconfig dqo emptyent req = do
    let dummyqo = setDummyEntityName dqo
    mfields <- readObjectInformation opts dummyqo
    case mfields of
      Nothing -> do
        printDetailMessage opts $
          "Entity " ++ quote dummyEntityName ++ " does not exist."
        getInfosConfig opts dqo reqs entityconfig emptyent
      Just jobject -> do
        printDetailMessage opts "Reading DUMMY object information succeeded."
        case lookupName req jobject of
          Just js -> maybe (return $ OutputError $
                              "Illegal value in field " ++ quote req ++
                              " in entity " ++ quotePrettyObject dummyqo)
                           (\qn -> do
                               printDetailMessage opts $ "Request " ++
                                  quote req ++ " exists in entity " ++
                                  quotePrettyObject dummyqo ++
                                  "Get infos from real name '" ++ qn ++ "'..."
                               return $ combineOutput [])
                           (fromJSON js)
          Nothing -> do
            printDetailMessage opts $ "Entity " ++ quotePrettyObject dummyqo ++
              " does not contain field " ++ quote req
            getInfosConfig opts dqo reqs entityconfig emptyent

  printDebugAndOutputError err = do printDetailMessage opts err
                                    generateOutputError opts err

  -- Return all entities of the query object currently stored in json files:
  queryAllStoredEntities :: QueryObject -> IO [String]
  queryAllStoredEntities qo = do
    let dir = objectDirectory opts qo
    exdir <- doesDirectoryExist dir
    if exdir then fmap (catMaybes . map jsonFile2Name)
                       (getDirectoryContents dir)
             else return []

  -- Return all entities of the query object.
  queryAllEntities pkg vsn m entqobj entreq =
    query (QueryModule pkg vsn m) entreq >>=
      maybe (return Nothing)
        (\opnames -> do
           let qopnames = map fromQName opnames
           mapM_ (\(mn,en) -> initializeObjectWithRealName opts
                                (setEName entqobj en) mn en)
                 (filter (not . null . fst) qopnames)
           stnames <- queryAllStoredEntities entqobj
           return $ Just $ 
             filter (/= dummyEntityName) $ union stnames $ map snd qopnames)

  query :: Read a => QueryObject -> String -> IO (Maybe a)
  query obj req = do
    printDetailMessage opts $ "Query for object " ++ quotePrettyObject obj ++
                              " and request " ++ quote req
    qopts <- getQueryOptions
    res <- getInfos qopts obj [req]
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
  fromOutputText out = case out of
    OutputText txt -> txt
    _              -> "ERROR IN TEXT OUTPUT FORMAT: " ++ show out

  fromOutputJSON :: Output -> JValue
  fromOutputJSON out = case out of
    OutputJSON jv -> jv
    _             -> JString $ "ERROR IN JSON OUTPUT FORMAT: " ++ show out

  fromOutputTerm :: Output -> [(String, String)]
  fromOutputTerm out = case out of
    OutputTerm ts -> ts
    _             -> [("ERROR", "ERROR IN TERM OUTPUT FORMAT: " ++ show out)]

  checkPackageExists :: Package -> IO Bool
  checkPackageExists pkg = do
    let jpath = objectJSONPath opts (QueryPackage pkg)
    whenFileDoesNotExist jpath $do
      path <- getCPMIndex
      printDebugMessage opts $ "Looking for package in index..."
      doesDirectoryExist (path </> pkg)

  checkVersionExists :: Package -> Version -> IO Bool
  checkVersionExists pkg vsn = do
    let jpath = objectJSONPath opts (QueryVersion pkg vsn)
    whenFileDoesNotExist jpath $ do
      path <- getCPMIndex
      printDebugMessage opts $ "Looking for version in index..."
      doesDirectoryExist (path </> pkg </> vsn)

  checkModuleExists :: Package -> Version -> Module -> IO Bool
  checkModuleExists pkg vsn m = do
    let jpath = objectJSONPath opts (QueryModule pkg vsn m)
    whenFileDoesNotExist jpath $ do
      allmods <- readPackageModules opts pkg vsn
      return (elem m allmods)

  ensureEntityExists pkg vsn m e qo ereq ename cont
    | isDummyObject qo = cont
    | otherwise
    = do
      exf <- doesFileExist (objectJSONPath opts qo)
      if exf
        then msgCont
        else do
          res <- query (QueryModule pkg vsn m) ereq
          case res of
            Nothing -> returnError
            Just es ->
              if e `elem` es
                then msgCont
                else maybe returnError
                          (\(mn,en) ->
                             initializeObjectWithRealName opts qo mn en >> cont)
                          (find ((== e) . snd) (map fromQName es))
   where
    msgCont = printDetailMessage opts
                (ename ++ " " ++ quotePrettyObject qobj ++ " exists.") >> cont
    returnError = printDebugAndOutputError $ unwords
      [ ename, quote e, "of module", quote m, "of version", quote vsn
      , "of package", quote pkg, "is not exported."]

whenFileDoesNotExist :: FilePath -> IO Bool -> IO Bool
whenFileDoesNotExist path act = do
  exf <- doesFileExist path
  case exf of True  -> return True
              False -> act

--- Check whether the given requests exist in the registered requests.
--- If this is not the case, return with an error message,
--- otherwise proceed with the continuation provided as the last argument.
checkRequests :: Show a =>
   Options -> [String] -> [RegisteredRequest a] -> IO Output -> IO Output
checkRequests opts reqs config cont = do
  let unknownrequests = reqs \\ map request config
  if null unknownrequests
    then cont
    else do let err = "Request" ++
                      (if length unknownrequests == 1
                         then " " ++ head unknownrequests ++ " is"
                         else "s " ++ unwords unknownrequests ++ " are") ++
                      " unknown!"
            printDetailMessage opts err
            generateOutputError opts err

--- This action process the given requests for the given query object
--- w.r.t. to the configuration for the kind of query object and
--- returns the output for the requests.
getInfosConfig :: Show a => Options -> QueryObject -> [String]
               -> [RegisteredRequest a] -> a -> IO Output
getInfosConfig opts queryobject reqs conf configobject =
  checkRequests opts reqs conf $
    getCheckedInfosConfig opts queryobject reqs conf configobject
 
getCheckedInfosConfig :: Show a => Options -> QueryObject -> [String]
                      -> [RegisteredRequest a] -> a -> IO Output
getCheckedInfosConfig opts queryobject reqs conf configobject
 | isDummyObject queryobject && optForce opts == 0
 = return $ createOutput opts queryobject []
 | isDummyObject queryobject
 = do -- run only the generator without considering the query object
   printDetailMessage opts "Generating requested information for dummy object..."
   mapM_ (generateDummyRequest configobject)
         (map (map toLower) reqs)
   return $ createOutput opts queryobject []
 | otherwise
 = do
  initializeObject opts queryobject
  printDetailMessage opts $
    "Reading current information of entity " ++ quotePrettyObject queryobject
  mfields <- readObjectInformation opts queryobject
  case mfields of
    Nothing -> do
      printErrorMessage $ errorReadingObject queryobject
      return $ OutputError $ errorReadingObject queryobject
    Just jobject -> do
      printDetailMessage opts "Reading information succeeded."
      let fields = fromJObject jobject
      case lookup realNameField fields of
       Just js -> maybe (return $ OutputError $
                           "Illegal value in field " ++ quote realNameField ++
                           " in entity " ++ quotePrettyObject queryobject)
                        (\qn -> do  -- forward to real name of entity:
                            printDetailMessage opts $
                              "Get infos from real name '" ++ qn ++ "'..."
                            let (mn,en) = fromQName qn
                            getInfos opts (setModEName queryobject mn en) reqs)
                        (fromJSON js)
       Nothing ->
        case optShowAll opts of
          True -> do
            printDetailMessage opts
              "Returning all currently available information..."
            let (okreqs,noreqs) = partition (isJust . snd)
                                    (map (\fn -> (fn, lookupRequest fn conf))
                                         (sort (map fst fields)))
            unless (null noreqs) $ do
              printErrorMessage $
                "Warning: entity has fields that are not a request: " ++
                unwords (map fst noreqs) ++ " (ignored)"
            let allReqs = catMaybes (map snd okreqs)
            results <- zipWithM
                        (\(_, _, extractor, _) fieldname ->
                          fmap (\x -> (fieldname, x))
                            (extractRequest opts fieldname extractor fields))
                        allReqs
                        (map fst okreqs) :: IO[(String, Maybe (JValue, String))]
            let results' = map (\(r, mr) ->
                                (r,
                                  maybe RequestUnknown
                                        (uncurry RequestResult) mr))
                              results
            return $ createOutput opts queryobject results'
          False -> do
            printDetailMessage opts $
              "Extracting/Generating requests '" ++ unwords reqs ++ "' for " ++
              quotePrettyObject queryobject
            results <- mapM (extractOrGenerate fields configobject
                                (errorRequestObject queryobject))
                            (map (map toLower) reqs)
                              :: IO [(String, RequestResult)]

            let newInformation = createNewInformation results
            unless (null newInformation) $
              updateObjectInformation opts queryobject newInformation
            return $ createOutput opts queryobject results
 where
  -- translate successfully computed information into corresponding fields
  createNewInformation :: [(String, RequestResult)] -> [(String, JValue)]
  createNewInformation =
    foldr (\(r, ir) acc ->
            fromRequestResult acc (const acc) (\jv _ -> (r, jv):acc) ir) []

  -- generate information for request only (used for dummy objects)
  generateDummyRequest obj req = do
    let reqerrormsg = errorRequestObject queryobject
    printDetailMessage opts $ "\nProcessing request '" ++ req ++ "'..."
    case lookupRequest req conf of
      Nothing -> printStatusMessage opts $ reqerrormsg req
      Just (_, _, _, generator) -> do
        printDetailMessage opts "Request found in configuration, generate..."
        generationResult <- generateRequest opts req generator obj
        case generationResult of
          Nothing -> printErrorMessage $
                       "EXECUTING REQUEST '" ++ req ++ "' FAILED!"
          Just _  -> do
            let dummyqobject = setDummyEntityName queryobject
            updateObjectInformation opts dummyqobject [(req, JString "ok")]
            printDetailMessage opts "Executing request succeeded"

  -- extract or generating (depending on force) information for request
  extractOrGenerate fields obj reqerrormsg req = do
    printDetailMessage opts $
      "\nProcessing request '" ++ req ++ "' for " ++ show obj ++ "..."
    case lookupRequest req conf of
      Nothing -> do
        printErrorMessage $ reqerrormsg req
        return (req, RequestError "UNDEFINED REQUEST!")
      Just (_, _, extractor, generator) -> do
        printDetailMessage opts
          "Request found in configuration. Looking at Force option..."
        case optForce opts of
          0 -> do
            printDebugMessage opts "Force option 0: Only extraction"
            extractionResult <- extractRequest opts req extractor fields
            case extractionResult of
              Nothing -> do
                printDetailMessage opts $
                  "Request '" ++ req ++ "' unknown for " ++ show obj
                return (req, RequestUnknown)
              Just (jv, output) -> do
                printDetailMessage opts "Executing request succeeded."
                return (req, RequestResult jv output)
          1 -> do
            printDebugMessage opts
              "Force option 1: Extraction, then generation if failed"
            extractionResult <- extractRequest opts req extractor fields
            case extractionResult of
              Nothing -> do
                generationResult <- generateRequest opts req generator obj
                case generationResult of
                  Nothing -> do
                    printDetailMessage opts $ "Request '" ++ req ++
                      "' could not be generated for " ++ show obj
                    return (req,
                            RequestError "EXTRACTING AND GENERATING FAILED")
                  Just (jv, output) -> do
                    printDetailMessage opts "Executing request succeeded."
                    return (req, RequestResult jv output)
              Just (jv, output) -> do
                printDetailMessage opts "Executing request succeeded."
                return (req, RequestResult jv output)
          2 -> do
            printDebugMessage opts "Force option 2: Only generation"
            generationResult <- generateRequest opts req generator obj
            case generationResult of
              Nothing -> do
                printDetailMessage opts $ "Request " ++ quote req ++
                  " could not be generated for " ++ show obj
                return (req, RequestError "GENERATING FAILED")
              Just (jv, output) -> do
                printDetailMessage opts "Executing request succeeded."
                return (req, RequestResult jv output)
          v -> do
            let msg = "ERROR: INVALID FORCE OPTION: " ++ show v
            printErrorMessage msg
            return (req, RequestError msg)

extractRequest :: Options -> String
               -> (Options -> [(String, JValue)] -> IO (Maybe (JValue, String)))
               -> [(String, JValue)] -> IO (Maybe (JValue, String))
extractRequest opts req extractor fields = do
  printDebugMessage opts "Trying extraction..."
  extractionResult <- extractor opts fields
  case extractionResult of
    Nothing -> do
      printDetailMessage opts $
        "Extraction failed: no request " ++ quote req ++ " in entity"
      return Nothing
    Just (jv, output) -> do
      printDetailMessage opts "Extraction succeeded."
      return $ Just (jv, output)
  
generateRequest :: Show a => Options -> String
                -> (Options -> a -> IO (Maybe (JValue, String))) -> a
                -> IO (Maybe (JValue, String))
generateRequest opts req generator obj = do
  printDebugMessage opts $
    "Trying generation of request '" ++ req ++ "' for " ++ show obj
  generationResult <- generator opts obj
  case generationResult of
    Nothing -> do
      printErrorMessage $ 
        "GENERATION OF REQUEST '" ++ req ++ "' FAILED FOR " ++ show obj
      return Nothing
    Just (jv, output) -> do
      printDetailMessage opts "Generation succeeded."
      return $ Just (jv, output)

-- Creates output of the info fields of an object w.r.t. the desired
-- output format.
createOutput :: Options -> QueryObject -> [(String, RequestResult)]
             -> Output
createOutput opts obj results = case optOutFormat opts of
  OutText -> OutputText $ unlines $
              (if outputSingleEntity then []
                                     else [object2StringTuple obj]) ++
              map (\(r, ir) -> withColor opts green r ++ ": " ++ 
                  fromRequestResult (withColor opts red "?") id (flip const) ir)
                  results
  OutJSON -> OutputJSON $ JObject $ toJObject $
                [("object", (JString . object2StringTuple) obj),
                  ("results",
                   JObject $ toJObject
                     (map (\(r, ir) ->
                              (r, fromRequestResult JNull JString
                                        (\_ s -> JString s) ir))
                          results))]
  OutTerm -> OutputTerm
                [(object2StringTuple obj,
                  show (map (\(r, ir) ->
                                (r, fromRequestResult "?" id (flip const) ir))
                            results))]
 where
  -- generate output for a single entity?
  outputSingleEntity = not
    (optAllTypes opts || optAllClasses opts || optAllOperations opts)
  
