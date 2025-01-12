------------------------------------------------------------------------------
--- This modules defines operations to generate information for the requests.
------------------------------------------------------------------------------

module CurryInfo.Generator where

import CurryInfo.Types
import CurryInfo.Paths
import CurryInfo.JConvert
import CurryInfo.Checkout ( toCheckout, getCheckoutPath, initializeCheckouts
                          , checkoutIfMissing)
import CurryInfo.Interface
  ( readInterface
  , getDeclarations
  , getOperations, getOperationQName, getOperationDecl
  , getOperationSignature, getInfixDecl, getOperationInfix
  , getOperationPrecedence
  , getAllTypes, getTypeQName, getHiddenTypes, getHiddenTypeQName, getTypeDecl
  , getTypeConstructors
  , getAllClasses, getClassQName, getHiddenClasses, getHiddenClassQName
  , getClassDecl, getClassMethods
  )
import CurryInfo.Helper     ( isCurryID, quote )
import CurryInfo.Analysis
import CurryInfo.SourceCode ( SourceCode, readSourceCode, readDocumentation
                            , getSourceCodeRef )
import CurryInfo.Parser     ( parseVersionConstraints )
import CurryInfo.Verbosity  ( printStatusMessage, printDetailMessage
                            , printDebugMessage)

import Text.Pretty (text)

import JSON.Data
import JSON.Parser (parseJSON)
import JSON.Convert
import JSON.Pretty

import System.IOExts    ( evalCmd )
import System.Directory ( doesDirectoryExist, doesFileExist
                        , getDirectoryContents )
import System.CurryPath ( curryModulesInDirectory )
import System.FilePath  ( (</>), (<.>) )
import System.IOExts    ( readCompleteFile )
import Data.Either      ( partitionEithers )
import Data.List        ( find, isPrefixOf, intersect, (\\) )
import Data.Maybe       ( catMaybes, maybeToList )

import Control.Monad    ( unless, when )

import DetParse (parse)

import CurryInterface.Types (Interface)

-- PACKAGE

gPackageName :: Generator CurryPackage String
gPackageName opts (CurryPackage pkg) = do
  printDetailMessage opts $ "Generating name for package '" ++ pkg ++ "'..."
  printDebugMessage opts $ "Name is: " ++ pkg
  finishResult opts pkg

gPackageVersions :: Generator CurryPackage [String]
gPackageVersions opts (CurryPackage pkg) = do
  printDetailMessage opts $ "Generating versions for package '" ++ pkg ++ "'..."
  printDetailMessage opts "Looking for package directory in index of package manager..."
  i <- getCPMIndex
  let packageDir = i </> pkg
  printDetailMessage opts $ "Directory in index is: " ++ packageDir
  printDetailMessage opts "Reading content of directory..."
  contents <- getReducedDirectoryContents packageDir
  printDebugMessage opts $ "Versions found: " ++ show contents
  let res = contents
  printDetailMessage opts "Generating finished successfully."
  return $ Just res

-- VERSION

gVersionVersion :: Generator CurryVersion String
gVersionVersion opts (CurryVersion pkg vsn) = do
  printDetailMessage opts $ "Generating version number for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  printDebugMessage opts $ "Version number is: " ++ vsn
  finishResult opts vsn

gVersionDocumentation :: Generator CurryVersion String
gVersionDocumentation opts (CurryVersion pkg vsn) = do
  printDetailMessage opts $ "Generating documentation for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  path <- packageREADMEPath opts pkg vsn >>= stripRootPath
  printDetailMessage opts "Generating finished successfully."
  return $ Just path

gVersionCategories :: Generator CurryVersion [String]
gVersionCategories =
  generateFromPackageJSON "categories" (\jv -> maybe [] id (getCategories jv))

gVersionModules :: Generator CurryVersion [String]
gVersionModules opts x@(CurryVersion pkg vsn) = do
  allMods <- readPackageModules opts pkg vsn
  generateFromPackageJSON "modules" (modulesSelector allMods) opts x
 where
  modulesSelector allMods jv =
    maybe allMods (intersect allMods) (getExportedModules jv)

gVersionDependencies :: Generator CurryVersion [Dependency]
gVersionDependencies =
  generateFromPackageJSON "dependencies" (\jv -> maybe [] id (getDependencies jv))

-- MODULE

gModuleName :: Generator CurryModule String
gModuleName opts (CurryModule pkg vsn m) = do
  printDetailMessage opts $
    "Generating name for module '" ++ m ++ "' of version '" ++ vsn ++
    "' of package '" ++ pkg ++ "'..."
  printDebugMessage opts $ "Name is: " ++ m
  finishResult opts m

gModuleDocumentation :: Generator CurryModule Reference
gModuleDocumentation opts x@(CurryModule pkg vsn m) = do
  printDetailMessage opts $
    "Generating documentation for module '" ++ m ++ "' of version '" ++ vsn ++
    "' of package '" ++ pkg ++ "'..."
  generateDocumentation opts x

gModuleSourceCode :: Generator CurryModule Reference
gModuleSourceCode opts x@(CurryModule pkg vsn m) = do
  printDetailMessage opts $
    "Generating source code for module '" ++ m ++ "' of version '" ++ vsn ++
    "' of package '" ++ pkg ++ "'..."
  generateSourceCode opts x

gModuleUnsafeModule :: Generator CurryModule String
gModuleUnsafeModule opts (CurryModule pkg vsn m) = do
  printDetailMessage opts $
    "Generating safe analysis for module '" ++ m ++ "' of version '" ++ vsn ++
    "' of package '" ++ pkg ++ "'..."
  mres <- analyseUnsafeModuleWithCASS opts pkg vsn m
  processAnalysisResult opts mres

gModuleClasses :: Generator CurryModule [String]
gModuleClasses opts (CurryModule pkg vsn m) = do
  printDetailMessage opts $
    "Generating exported classes for module '" ++ m ++ "' of version '" ++
    vsn ++ "' of package '" ++ pkg ++ "'..."
  mbns <- generateFromInterface pkg vsn m "classes" classesSelector opts
  return (mbns >>= Just . map qName2String)
 where
  classesSelector interface =
    let allClasses    = catMaybes $ map getClassQName $ getAllClasses $
                          getDeclarations interface
        hiddenClasses = catMaybes $ map getHiddenClassQName $ getHiddenClasses $
                          getDeclarations interface
    in Just (allClasses \\ hiddenClasses)

gModuleTypes :: Generator CurryModule [String]
gModuleTypes opts (CurryModule pkg vsn m) = do
  printDetailMessage opts $
    "Generating exported types for module '" ++ m ++ "' of version '" ++
    vsn ++ "' of package '" ++ pkg ++ "'..."
  mbns <- generateFromInterface pkg vsn m "types" typesSelector opts
  return (mbns >>= Just . map qName2String)
 where
  typesSelector interface =
    let allTypes    = catMaybes $ map getTypeQName $ getAllTypes $
                        getDeclarations interface
        hiddenTypes = catMaybes $ map getHiddenTypeQName $ getHiddenTypes $
                        getDeclarations interface
    in Just (allTypes \\ hiddenTypes)

gModuleOperations :: Generator CurryModule [String]
gModuleOperations opts (CurryModule pkg vsn mn) = do
  printDetailMessage opts $
    "Generating exported operations for module '" ++ mn ++ "' of version '" ++
    vsn ++ "' of package '" ++ pkg ++ "'..."
  mbns <- generateFromInterface pkg vsn mn "operations" operationsSelector opts
  return (mbns >>= Just . map qName2String)
 where
  operationsSelector interface = Just $ catMaybes $
    map getOperationQName $ getOperations $ getDeclarations interface

qName2String :: (String,String) -> String
qName2String (m,n) = if null m then n else m ++ "." ++ n

-- TYPE

gTypeName :: Generator CurryType String
gTypeName opts (CurryType pkg vsn m t) = do
  printDetailMessage opts $
    "Generating name for type '" ++ t ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  printDebugMessage opts $ "Name is: " ++ t
  finishResult opts t

gTypeDocumentation :: Generator CurryType Reference
gTypeDocumentation opts x@(CurryType pkg vsn m t) = do
  printDetailMessage opts $
    "Generating documentation for type '" ++ t ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
  generateDocumentation opts x

gTypeConstructors :: Generator CurryType [String]
gTypeConstructors opts (CurryType pkg vsn m t) = do
  printDetailMessage opts $
    "Generating constructors for type '" ++ t ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  generateFromInterface pkg vsn m "constructors" constructorsSelector opts
 where
  constructorsSelector interface =
    getTypeDecl t (getAllTypes $ getDeclarations interface)
      >>= getTypeConstructors

gTypeDefinition :: Generator CurryType Reference
gTypeDefinition opts x@(CurryType pkg vsn m t) = do
  printDetailMessage opts $
    "Generating definition for type '" ++ t ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
  generateSourceCode opts x

-- TYPECLASS

gClassName :: Generator CurryClass String
gClassName opts (CurryClass pkg vsn m c) = do
  printDetailMessage opts $
    "Generating name of class '" ++ c ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  printDebugMessage opts $ "Name is: " ++ c
  finishResult opts c

gClassDocumentation :: Generator CurryClass Reference
gClassDocumentation opts x@(CurryClass pkg vsn m c) = do
  printDetailMessage opts $
    "Generating documentation of class '" ++ c ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  generateDocumentation opts x

gClassMethods :: Generator CurryClass [String]
gClassMethods opts (CurryClass pkg vsn m c) = do
    printDetailMessage opts $
      "Generating methods of class '" ++ c ++ "' of module '" ++ m ++
      "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    generateFromInterface pkg vsn m "methods" methodsSelector opts
  where
    methodsSelector interface =
      getClassDecl c (getAllClasses $ getDeclarations interface)
        >>= getClassMethods

gClassDefinition :: Generator CurryClass Reference
gClassDefinition opts x@(CurryClass pkg vsn m c) = do
  printDetailMessage opts $
    "Generating definition of class '" ++ c ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  generateSourceCode opts x

-- OPERATION

gOperationName :: Generator CurryOperation String
gOperationName opts (CurryOperation pkg vsn m o) = do
  printDetailMessage opts $
    "Generating name of operation '" ++ o ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  printDebugMessage opts $ "Name is: " ++ o
  finishResult opts o

gOperationDocumentation :: Generator CurryOperation Reference
gOperationDocumentation opts x@(CurryOperation pkg vsn m o) = do
  printDetailMessage opts $
    "Generating documentation of operation '" ++ o ++ "' of module '" ++ m ++
    "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
  generateDocumentation opts x

gOperationSourceCode :: Generator CurryOperation Reference
gOperationSourceCode opts x@(CurryOperation pkg vsn m o)
  | isCurryID o
  = do printDetailMessage opts $ "Generating source code of operation '" ++ o ++
         "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++
         pkg ++ "'..."
       generateSourceCode opts x
  | otherwise = return Nothing

gOperationSignature :: Generator CurryOperation Signature
gOperationSignature opts (CurryOperation pkg vsn m o) = do
  printDetailMessage opts $ "Generating signature of operation '" ++ o ++
    "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++
    pkg ++ "'..."
  generateFromInterface pkg vsn m "signature" signatureSelector opts
 where
  signatureSelector :: Interface -> Maybe Signature
  signatureSelector int =
    getOperationDecl o (getOperations $ getDeclarations int)
      >>= getOperationSignature

gOperationInfix :: Generator CurryOperation (Maybe Infix)
gOperationInfix opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating infix of operation '" ++ o ++
      "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++
      pkg ++ "'..."
    generateFromInterface pkg vsn m "infix" infixSelector opts
  where
    infixSelector interface =
      Just (getInfixDecl o (getDeclarations interface) >>= getOperationInfix)

gOperationPrecedence :: Generator CurryOperation (Maybe Precedence)
gOperationPrecedence opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating precedence of operation '" ++ o ++ 
      "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++
      pkg ++ "'..."
    generateFromInterface pkg vsn m "precedence" precedenceSelector opts
  where
    precedenceSelector interface =
      Just (getInfixDecl o (getDeclarations interface)
              >>= getOperationPrecedence :: Maybe Precedence)

gOperationCASSDeterministic :: Generator CurryOperation String
gOperationCASSDeterministic =
  generateOperationAnalysisWithCASS "deterministic" analyseDeterministicWithCASS

gOperationCASSDemand :: Generator CurryOperation String
gOperationCASSDemand =
  generateOperationAnalysisWithCASS "demand" analyseDemandWithCASS

gOperationCASSIndeterministic :: Generator CurryOperation String
gOperationCASSIndeterministic =
  generateOperationAnalysisWithCASS "indeterministic"
    analyseIndeterministicWithCASS

gOperationCASSSolComplete :: Generator CurryOperation String
gOperationCASSSolComplete =
  generateOperationAnalysisWithCASS "solution completeness"
    analyseSolCompleteWithCASS

gOperationCASSTerminating :: Generator CurryOperation String
gOperationCASSTerminating =
  generateOperationAnalysisWithCASS "terminating" analyseTerminatingWithCASS

gOperationCASSTotal :: Generator CurryOperation String
gOperationCASSTotal =
  generateOperationAnalysisWithCASS "totally defined" analyseTotalWithCASS

gOperationFailFree :: Generator CurryOperation String
gOperationFailFree =
  createInfoGeneratorWith "fail-free analysis" analyseFailFree

--------------------------------------------------------------------------

--- Generator function to create an information generator for package versions.
--- The first argument is a description of the generated information
--- and the second argument is the operation that looks for the information
--- in the package json file.
generateFromPackageJSON :: Show b => String -> (JValue -> b)
                        -> Generator CurryVersion b
generateFromPackageJSON desc selector opts (CurryVersion pkg vsn) = do
  printDetailMessage opts $ unwords $
    ["Generating", desc, "for version", quote vsn, "of package", quote pkg, "..."]
  mbdirjson <- readPackageJSON opts pkg vsn
  case mbdirjson of
    Nothing -> return Nothing
    Just (_,pkgjson) -> do
      printDebugMessage opts $ "JSON:\n" ++ ppJSON pkgjson
      let res = selector pkgjson
      printDebugMessage opts $ "Result: " ++ show res
      printDetailMessage opts "Generating finished successfully."
      return $ Just res

--- Generator function to get information from an interface.
--- The first three arguments are the package, the version and the module.
--- The fourth argument is a description of the generated information.
--- The fifth argument is the operation, that looks for the information
--- in the interface of the module.
generateFromInterface :: Show b => Package -> Version -> Module -> String
                      -> (Interface -> Maybe b) -> Options -> IO (Maybe b)
generateFromInterface pkg vsn m desc selector opts = do
  minterface <- readInterface opts pkg vsn m
  case minterface of
    Nothing -> do
      printDetailMessage opts "Failed to read interface."
      printDetailMessage opts "Generating failed."
      return Nothing
    Just interface -> do
      printDetailMessage opts $ "Reading " ++ desc ++ " from interface..."
      case selector interface of
        Nothing -> do
          printDetailMessage opts "Failed to find information in interface."
          printDetailMessage opts "Generating failed."
          return Nothing
        Just res -> do
          printDebugMessage opts $ "Result: " ++ show res
          printDetailMessage opts "Generating finished successfully."
          return $ Just res

--- Generator function to create an information generator for operations.
--- The first argument is a description of the generated information
--- and the second argument is the actual operation which generates
--- the information.
createInfoGeneratorWith :: Show a => String
  -> (Options -> Package -> Version -> Module -> Operation -> IO (Maybe a))
  -> Generator CurryOperation a
createInfoGeneratorWith anadescr anafun opts (CurryOperation pkg vsn m o) = do
  printDetailMessage opts $ "Generating " ++ anadescr ++ " of operation '" ++
    o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++
    "' of package '" ++ pkg ++ "'..."
  mres <- anafun opts pkg vsn m o
  processAnalysisResult opts mres

--- Generator function to get a reference information.
--- The first argument is the operation, that generates the reference.
generateReference :: SourceCode a => (Options -> a -> IO (Maybe Reference))
                  -> Generator a Reference
generateReference fun opts obj = do
  mres <- fun opts obj
  case mres of
    Nothing -> do
      printDetailMessage opts "Generating failed."
      return Nothing
    Just res -> do
      printDebugMessage opts $ "Result is: " ++ show res
      printDetailMessage opts "Generating finished successfully."
      return $ Just res

--- Generator function to get a reference to the documentation.
generateDocumentation :: SourceCode a => Generator a Reference
generateDocumentation = generateReference readDocumentation

--- Generator function to get a reference to the source code.
generateSourceCode :: SourceCode a => Generator a Reference
generateSourceCode = generateReference readSourceCode

--- Generator function to create an information generator using CASS for
--- the analysis.
--- The first argument is a description of the analysis
--- and the second argument is the name of the analysis given to CASS
--- as an argument.
generateOperationAnalysisWithCASS :: Show b => String
  -> (Options -> Package -> Version -> Module -> Operation -> IO (Maybe b))
  -> Generator CurryOperation b
generateOperationAnalysisWithCASS desc analysis opts
                                  (CurryOperation pkg vsn m o) = do
  printDetailMessage opts $ "Generating " ++ desc ++ " analysis of operation '"
    ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++
    "' of package '" ++ pkg ++ "'..."
  mres <- analysis opts pkg vsn m o
  processAnalysisResult opts mres

--- This action prints messages about the result depending on the current
--- options. It returns the result as a Maybe value.
finishResult :: Options -> String -> IO (Maybe String)
finishResult opts res = do
  printDebugMessage opts $ "Result is: " ++ res
  printDetailMessage opts "Generating finished successfully."
  return $ Just res

--- This action prints messages about the result depending on the current
--- options and whether a result even exists.
--- The result is returned unchanged.
processAnalysisResult :: Show b => Options -> Maybe b -> IO (Maybe b)
processAnalysisResult opts mres = case mres of
  Just res -> do
    printDebugMessage opts $ "Result is: " ++ show res
    printDetailMessage opts "Generating finished successfully."
    return $ Just res
  Nothing -> do
    printDetailMessage opts "Analysis failed."
    printDetailMessage opts "Generating failed."
    return Nothing

-- HELPER

lookupField :: String -> JValue -> Maybe JValue
lookupField s jv = case jv of
  JObject fields -> lookup s fields
  _ -> Nothing

--- Returns the `category` field in a JSON object, if present.
getCategories :: JValue -> Maybe [String]
getCategories jv = lookupField "category" jv >>= fromJSONList

--- Returns the `exportedModules` field in a JSON object, if present.
getExportedModules :: JValue -> Maybe [String]
getExportedModules jv = lookupField "exportedModules" jv >>= fromJSONList

--- Returns the `sourceDirs` field in a JSON object. If it is not present,
--- return the single directory `src`.
getSourceDirs :: JValue -> [String]
getSourceDirs jv = 
  maybe ["src"] id (lookupField "sourceDirs" jv >>= fromJSONList)

--- Returns the `dependencies` field in a JSON object, if present.
getDependencies :: JValue -> Maybe [Dependency]
getDependencies jv = case jv of
  JObject fields -> do
    value <- lookup "dependencies" fields
    case value of
      JObject fields' -> mapM convertDependency fields'
      _               -> Nothing
  _ -> Nothing

convertDependency :: (String, JValue) -> Maybe Dependency
convertDependency (pkg, jv) = do
  vcs <- fromJSON jv
  disj <- parseVersionConstraints vcs
  return (Dependency pkg disj)

readPackageModules :: Options -> Package -> Version -> IO [Module]
readPackageModules opts pkg vsn = do
  mbdirjson <- readPackageJSON opts pkg vsn
  case mbdirjson of
    Nothing  -> return []
    Just (dir,json) -> do
      let srcdirs = map (dir </>) (getSourceDirs json)
      fmap concat (mapM curryModulesInDirectory srcdirs)

--- Returns the directory and JSON value of `package.json` of a package with
--- the given version. `Nothing` is returned it it cannot be found
--- or cannot be parsed (which should not be the case).
readPackageJSON :: Options -> Package -> Version
                   -> IO (Maybe (String,JValue))
readPackageJSON opts pkg vsn = do
  printDetailMessage opts $
    "Reading package json of " ++ pkg ++ "-" ++ vsn ++ "..."
  result <- checkoutIfMissing opts pkg vsn
  case result of
    Nothing  -> return Nothing
    Just dir -> do
      let packageJSON = dir </> "package.json"
      b <- doesFileExist packageJSON
      case b of
        False -> do printDetailMessage opts "'package.json' not found!"
                    return Nothing
        True  -> do
          jsontxt <- readCompleteFile packageJSON
          case parseJSON jsontxt of
            Nothing   -> do printDetailMessage opts
                             "Failed to parse package.json."
                            return Nothing
            Just json -> return (Just (dir,json))

--- Returns the path of a `README` file of a package with the given version.
--- Returns the empty string if such a file does not exist.
packageREADMEPath :: Options -> Package -> Version -> IO String
packageREADMEPath opts pkg vsn = do
  printDetailMessage opts "Finding path to README..."
  result <- checkoutIfMissing opts pkg vsn
  case result of
    Nothing  -> return ""
    Just dir -> do
      dconts <- getDirectoryContents dir
      return $ maybe "" (dir </>) (find ("README" `isPrefixOf`) dconts)
