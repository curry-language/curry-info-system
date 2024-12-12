-----------------------------------------------------------------------------
--- This modules defines operations to create outputs and process requests
--- to get information about objects.
-----------------------------------------------------------------------------

module CurryInfo.Information where

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

--- This actions process the given requests for the given object and returns the output.
getInfos :: Options -> QueryObject -> [String] -> IO Output
getInfos opts qobj reqs = do
  printStatusMessage opts "Checking structure of the request..."
  case qobj of
    PackageObject pkg -> do
      printStatusMessage opts "Structure matches Package."
      let obj = CurryPackage pkg
      result <- checkPackageExists pkg
      case result of
        False -> do
          let err = "Package '" ++ pkg ++ "' does not exist."
          printDebugMessage opts err
          generateOutputError opts err
        True -> do
          printDebugMessage opts "Package exists."
          getInfos' packageConfiguration obj qobj
    VersionObject pkg vsn -> do
      printStatusMessage opts "Structure matches Version."
      let obj = CurryVersion pkg vsn
      result <- checkVersionExists pkg vsn
      case result of
        False -> do
          let err = "Version '" ++ vsn ++ "' of package '" ++ pkg ++
                    "' does not exists."
          printDetailMessage opts err
          generateOutputError opts err
        True -> do
          printDetailMessage opts "Checked Version: exists."
          getInfos' versionConfiguration obj qobj
    ModuleObject pkg vsn m -> do
      printStatusMessage opts "Structure matches Module."
      let obj = CurryModule pkg vsn m
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
                  outs <- mapM (\(o,o') -> getInfos' typeConfiguration o o')
                               (map (\t -> (CurryType pkg vsn m t, TypeObject pkg vsn m t)) ts)
                  let out = combineOutput outs
                  return out
            (_, True, _) -> do
              mcs <- queryAllTypeclasses pkg vsn m
              case mcs of
                Nothing -> do
                  generateOutputError opts "Could not find typeclasses"
                Just cs -> do
                  outs <- mapM (\(o,o') -> getInfos' typeclassConfiguration o o')
                               (map (\t -> (CurryTypeclass pkg vsn m t, TypeClassObject pkg vsn m t)) cs)
                  let out = combineOutput outs
                  return out
            (_, _, True) -> do
              mos <- queryAllOperations pkg vsn m
              case mos of
                Nothing -> do
                  generateOutputError opts "Could not find operations."
                Just os -> do
                  outs <- mapM (\(o,o') -> getInfos' operationConfiguration o o')
                               (map (\o -> (CurryOperation pkg vsn m o, OperationObject pkg vsn m o)) os)
                  let out = combineOutput outs
                  return out
            (False, False, False) -> do
              getInfos' moduleConfiguration obj qobj
    TypeObject pkg vsn m t -> do
      printStatusMessage opts "Structure matches Type"
      let obj = CurryType pkg vsn m t
      result <- checkTypeExists pkg vsn m t
      case result of
        False -> do
          let err = "Type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++
                    vsn ++ "' of package '" ++ pkg ++ "' is not exported."
          printDetailMessage opts err
          generateOutputError opts err
        True  -> do
          printDetailMessage opts "Checked Type: exists."
          getInfos' typeConfiguration obj qobj
    TypeClassObject pkg vsn m c -> do
      printStatusMessage opts "Structure matches Typeclass"
      let obj = CurryTypeclass pkg vsn m c
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
          getInfos' typeclassConfiguration obj qobj
    OperationObject pkg vsn m o -> do
      printStatusMessage opts "Structure matches Operation."
      let obj = CurryOperation pkg vsn m o
      result <- checkOperationExists pkg vsn m o
      case result of
        False -> do
          let err = "Operation '" ++ o ++ "' of module '" ++ m ++
                    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++
                    "' is not exported."
          printDetailMessage opts err
          generateOutputError opts err
        True  -> do
          printDetailMessage opts "Checker Operation: exists."
          getInfos' operationConfiguration obj qobj
  where
    getInfos' conf confobj qobj' = do
      printStatusMessage opts "Initializing Input..."
      initializeStore qobj'
      printStatusMessage opts "Reading current information..."
      mfields <- readObjectInformation opts qobj'
      case mfields of
        Nothing -> do
          printDetailMessage opts "Reading information failed."
          return $ OutputError $ errorReadingObject qobj'
        Just fields -> do
          printDetailMessage opts "Reading information succeeded."
          case optShowAll opts of
            True -> do
              printStatusMessage opts "Returning all currently available information..."
              let fieldNames = map fst fields
              case mapM (flip lookupRequest conf) fieldNames of
                Nothing -> do
                  return $ OutputError $ "One of the fields could not be found: " ++ show fieldNames
                Just allReqs -> do
                  results <- zipWithM (\(_, _, extractor, _) fieldName -> fmap (\x -> (fieldName, x)) (extract extractor fields)) allReqs fieldNames :: IO [(String, Maybe (JValue, String))]
                  let results' = map (\(r, mr) -> (r, maybe InformationExtractionFailed (uncurry InformationResult) mr)) results
                  let output = createOutput qobj' results' :: Output
                  return output
            False -> do
              printDetailMessage opts "Extracting/Generating requested information..."
              results <- mapM (extractOrGenerate conf fields confobj (errorRequestObject qobj'))
                              (map (map toLower) reqs) :: IO [(String, InformationResult)]

              let newInformation = createNewInformation results
              printDebugMessage opts "Overwriting with updated information..."
              writeObjectInformation qobj' (newInformation <+> fields)
              printDetailMessage opts "Overwriting finished."

              let output = createOutput qobj' results :: Output
              return output
    
    queryAllTypes :: Package -> Version -> Module -> IO (Maybe [Type])
    queryAllTypes pkg vsn m = query (ModuleObject pkg vsn m) "types"

    queryAllTypeclasses :: Package -> Version -> Module -> IO (Maybe [Typeclass])
    queryAllTypeclasses pkg vsn m = query (ModuleObject pkg vsn m) "typeclasses"

    queryAllOperations :: Package -> Version -> Module -> IO (Maybe [Operation])
    queryAllOperations pkg vsn m = query (ModuleObject pkg vsn m) "operations"
    
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

    extractOrGenerate :: Configuration a -> [(String, JValue)] -> a
                      -> (String -> String) -> String -> IO (String, InformationResult)
    extractOrGenerate conf fields obj reqerrormsg req = do
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
              extractionResult <- extract extractor fields
              case extractionResult of
                Nothing -> do
                  printDetailMessage opts "Executing request failed"
                  return (req, InformationExtractionFailed)
                Just (jv, output) -> do
                  printDetailMessage opts "Executing request succeeded."
                  return (req, InformationResult jv output)
            1 -> do
              printDebugMessage opts "Force option 1: Extraction, then generation if failed"
              extractionResult <- extract extractor fields
              case extractionResult of
                Nothing -> do
                  generationResult <- generate generator obj
                  case generationResult of
                    Nothing -> do
                      printDetailMessage opts "Executing request failed."
                      return (req, InformationError "EXTRACTING AND GENERATING FAILED")
                    Just (jv, output) -> do
                      printDetailMessage opts "Executing request succeeded."
                      return (req, InformationResult jv output)
                Just (jv, output) -> do
                  printDetailMessage opts "Executing request succeeded."
                  return (req, InformationResult jv output)
            2 -> do
              printDebugMessage opts "Force option 2: Only generation"
              generationResult <- generate generator obj
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

    extract :: (Options -> [(String, JValue)] -> IO (Maybe (JValue, String))) -> [(String, JValue)] -> IO (Maybe (JValue, String))
    extract extractor fields = do
      printDebugMessage opts "Trying extraction..."
      extractionResult <- extractor opts fields
      case extractionResult of
        Nothing -> do
          printDetailMessage opts "Extraction failed."
          return Nothing
        Just (jv, output) -> do
          printDetailMessage opts "Extraction succeeded."
          return $ Just (jv, output)
    
    generate :: (Options -> a -> IO (Maybe (JValue, String))) -> a -> IO (Maybe (JValue, String))
    generate generator obj = do
      printDebugMessage opts "Trying generation..."
      generationResult <- generator opts obj
      case generationResult of
        Nothing -> do
          printDetailMessage opts "Generation failed."
          return Nothing
        Just (jv, output) -> do
          printDetailMessage opts "Generation succeeded."
          return $ Just (jv, output)
    
    checkPackageExists :: Package -> IO Bool
    checkPackageExists pkg = do
      jpath <- getJSONPath (PackageObject pkg)
      b1 <- doesFileExist jpath
      case b1 of
        True -> return True
        False -> do
          path <- index
          printDebugMessage opts $ "Looking for package in index..."
          b2 <- doesDirectoryExist (path </> pkg)
          return b2

    checkVersionExists :: Package -> Version -> IO Bool
    checkVersionExists pkg vsn = do
      jpath <- getJSONPath (VersionObject pkg vsn)
      b1 <- doesFileExist jpath
      case b1 of
        True -> return True
        False -> do
          path <- index
          printDebugMessage opts $ "Looking for version in index..."
          b2 <- doesDirectoryExist (path </> pkg </> vsn)
          return b2

    checkModuleExists :: Package -> Version -> Module -> IO Bool
    checkModuleExists pkg vsn m = do
      jpath <- getJSONPath (ModuleObject pkg vsn m)
      b1 <- doesFileExist jpath
      case b1 of
        True -> return True
        False -> do
          allMods <- readPackageModules opts pkg vsn

          packageJSON <- readPackageJSON opts pkg vsn
          let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

          return (elem m exportedMods)

    checkTypeExists :: Package -> Version -> Module -> Type -> IO Bool
    checkTypeExists pkg vsn m t = do
      jpath <- getJSONPath (TypeObject pkg vsn m t)
      b1 <- doesFileExist jpath
      case b1 of
        True -> return True
        False -> do
          res <- query (ModuleObject pkg vsn m) "types"
          case res of
            Nothing -> return False
            Just ts -> return $ elem t ts

    checkTypeclassExists :: Package -> Version -> Module -> Typeclass -> IO Bool
    checkTypeclassExists pkg vsn m c = do
      jpath <- getJSONPath (TypeClassObject pkg vsn m c)
      b1 <- doesFileExist jpath
      case b1 of
        True -> return True
        False -> do
          res <- query (ModuleObject pkg vsn m) "typeclasses"
          case res of
            Nothing -> return False
            Just cs -> return $ elem c cs

    checkOperationExists :: Package -> Version -> Module -> Operation -> IO Bool
    checkOperationExists pkg vsn m o = do
      jpath <- getJSONPath (OperationObject pkg vsn m o)
      b1 <- doesFileExist jpath
      case b1 of
        True -> return True
        False -> do
          res <- query (ModuleObject pkg vsn m) "operations"
          case res of
            Nothing -> return False
            Just os -> return $ elem o os

    query :: Read a => QueryObject -> String -> IO (Maybe a)
    query obj req = do
      res <- getInfos queryOptions obj [req] :: IO Output
      case res of
        -- OutputTerm [("obj", [("req", "res")])]
        OutputTerm [(_, x)] -> case lookup req (read x) of
          Nothing -> return Nothing
          Just y -> return (Just (read y))
        _ -> return Nothing
