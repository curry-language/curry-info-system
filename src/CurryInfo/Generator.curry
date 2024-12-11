-----------------------------------------------------------------------------------------
--- This modules defines operations to generate information for the requests.
-----------------------------------------------------------------------------------------

module CurryInfo.Generator where

import CurryInfo.Types
import CurryInfo.Paths
import CurryInfo.JConvert
import CurryInfo.Checkout (toCheckout, getCheckoutPath, initializeCheckouts, checkoutIfMissing)
import CurryInfo.Interface 
    ( readInterface
    , getDeclarations
    , getOperations, getOperationName, getOperationDecl, getOperationSignature
    , getInfixDecl, getOperationInfix
    , getOperationPrecedence
    , getAllTypes, getTypeName, getHiddenTypes, getHiddenTypeName, getTypeDecl, getTypeConstructors
    , getAllClasses, getClassName, getHiddenClasses, getHiddenClassName, getClassDecl, getClassMethods
    )
import CurryInfo.Analysis
import CurryInfo.SourceCode (SourceCode, readSourceCode, readDocumentation)
import CurryInfo.Parser (parseVersionConstraints)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)

import Text.Pretty (text)

import JSON.Data
import JSON.Parser (parseJSON)
import JSON.Convert
import JSON.Pretty

import System.IOExts (evalCmd)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.CurryPath (curryModulesInDirectory, modNameToPath)
import System.FilePath ((</>), (<.>))

import Data.List (isPrefixOf, intersect, (\\))
import Data.Maybe (catMaybes)

import Control.Monad (when)

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
    i <- index
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
    path <- packageREADMEPath opts pkg vsn
    let res = path
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gVersionCategories :: Generator CurryVersion [String]
gVersionCategories =
    generateFromPackageJSON "categories" (\jv -> maybe [] id (getCategories jv))

gVersionModules :: Generator CurryVersion [String]
gVersionModules opts x@(CurryVersion pkg vsn) = do
        allMods <- readPackageModules opts pkg vsn
        generateFromPackageJSON "modules" (modulesSelector allMods) opts x
    where
        modulesSelector allMods jv = maybe allMods (intersect allMods) (getExportedModules jv)

gVersionDependencies :: Generator CurryVersion [Dependency]
gVersionDependencies =
    generateFromPackageJSON "dependencies" (\jv -> maybe [] id (getDependencies jv))

-- MODULE

gModuleName :: Generator CurryModule String
gModuleName opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating name for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ m
    finishResult opts m

gModuleDocumentation :: Generator CurryModule Reference
gModuleDocumentation opts x@(CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating documentation for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    generateDocumentation opts x

gModuleSourceCode :: Generator CurryModule Reference
gModuleSourceCode opts x@(CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating source code for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    generateSourceCode opts x

gModuleUnsafeModule :: Generator CurryModule String
gModuleUnsafeModule opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating safe analysis for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mres <- analyseUnsafeModuleWithCASS opts pkg vsn m
    processAnalysisResult opts mres

gModuleTypeclasses :: Generator CurryModule [String]
gModuleTypeclasses opts (CurryModule pkg vsn m) = do
        printDetailMessage opts $ "Generating exported typeclasses for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "typeclasses" typeclassesSelector opts
    where
        typeclassesSelector interface = let allClasses = catMaybes $ map getClassName $ getAllClasses $ getDeclarations interface
                                            hiddenClasses = catMaybes $ map getHiddenClassName $ getHiddenClasses $ getDeclarations interface
                                        in Just (allClasses \\ hiddenClasses)

gModuleTypes :: Generator CurryModule [String]
gModuleTypes opts (CurryModule pkg vsn m) = do
        printDetailMessage opts $ "Generating exported types for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "types" typesSelector opts
    where
        typesSelector interface = let allTypes = catMaybes $ map getTypeName $ getAllTypes $ getDeclarations interface
                                      hiddenTypes = catMaybes $ map getHiddenTypeName $ getHiddenTypes $ getDeclarations interface
                                  in Just (allTypes \\ hiddenTypes)

gModuleOperations :: Generator CurryModule [String]
gModuleOperations opts (CurryModule pkg vsn m) = do
        printDetailMessage opts $ "Generating exported operations for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "operations" operationsSelector opts
    where
        operationsSelector interface = Just (catMaybes $ map getOperationName $ getOperations $ getDeclarations interface)

-- TYPE

gTypeName :: Generator CurryType String
gTypeName opts (CurryType pkg vsn m t) = do
    printDetailMessage opts $ "Generating name for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ t
    finishResult opts t

gTypeDocumentation :: Generator CurryType Reference
gTypeDocumentation opts x@(CurryType pkg vsn m t) = do
    printDetailMessage opts $ "Generating documentation for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    generateDocumentation opts x

gTypeConstructors :: Generator CurryType [String]
gTypeConstructors opts (CurryType pkg vsn m t) = do
        printDetailMessage opts $ "Generating constructors for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "constructors" constructorsSelector opts
    where
        constructorsSelector interface = getTypeDecl t (getAllTypes $ getDeclarations interface) >>= getTypeConstructors

gTypeDefinition :: Generator CurryType Reference
gTypeDefinition opts x@(CurryType pkg vsn m t) = do
    printDetailMessage opts $ "Generating definition for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    generateSourceCode opts x

-- TYPECLASS

gTypeclassName :: Generator CurryTypeclass String
gTypeclassName opts (CurryTypeclass pkg vsn m c) = do
    printDetailMessage opts $ "Generating name of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ c
    finishResult opts c

gTypeclassDocumentation :: Generator CurryTypeclass Reference
gTypeclassDocumentation opts x@(CurryTypeclass pkg vsn m c) = do
    printDetailMessage opts $ "Generating documentation of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    generateDocumentation opts x

gTypeclassMethods :: Generator CurryTypeclass [String]
gTypeclassMethods opts (CurryTypeclass pkg vsn m c) = do
        printDetailMessage opts $ "Generating methods of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "methods" methodsSelector opts
    where
        methodsSelector interface = getClassDecl c (getAllClasses $ getDeclarations interface) >>= getClassMethods

gTypeclassDefinition :: Generator CurryTypeclass Reference
gTypeclassDefinition opts x@(CurryTypeclass pkg vsn m c) = do
    printDetailMessage opts $ "Generating definition of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    generateSourceCode opts x

-- OPERATION

gOperationName :: Generator CurryOperation String
gOperationName opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating name of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ o
    finishResult opts o

gOperationDocumentation :: Generator CurryOperation Reference
gOperationDocumentation opts x@(CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating documentation of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    generateDocumentation opts x

gOperationSourceCode :: Generator CurryOperation Reference
gOperationSourceCode opts x@(CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating source code of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    generateSourceCode opts x

gOperationSignature :: Generator CurryOperation Signature
gOperationSignature opts (CurryOperation pkg vsn m o) = do
        printDetailMessage opts $ "Generating signature of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "signature" signatureSelector opts
    where
        signatureSelector :: Interface -> Maybe Signature
        signatureSelector interface = getOperationDecl o (getOperations $ getDeclarations interface) >>= getOperationSignature

gOperationInfix :: Generator CurryOperation (Maybe Infix)
gOperationInfix opts (CurryOperation pkg vsn m o) = do
        printDetailMessage opts $ "Generating infix of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "infix" infixSelector opts
    where
        infixSelector interface = Just (getInfixDecl o (getDeclarations interface) >>= getOperationInfix)

gOperationPrecedence :: Generator CurryOperation (Maybe Precedence)
gOperationPrecedence opts (CurryOperation pkg vsn m o) = do
        printDetailMessage opts $ "Generating precedence of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
        generateFromInterface pkg vsn m "precedence" precedenceSelector opts
    where
        precedenceSelector interface = Just (getInfixDecl o (getDeclarations interface) >>= getOperationPrecedence :: Maybe Precedence)

gOperationCASSDeterministic :: Generator CurryOperation String
gOperationCASSDeterministic =
    generateOperationAnalysisWithCASS "deterministic" analyseDeterministicWithCASS

gOperationCASSDemand :: Generator CurryOperation String
gOperationCASSDemand =
    generateOperationAnalysisWithCASS "demand" analyseDemandWithCASS

gOperationCASSIndeterministic :: Generator CurryOperation String
gOperationCASSIndeterministic =
    generateOperationAnalysisWithCASS "indeterministic" analyseIndeterministicWithCASS

gOperationCASSSolComplete :: Generator CurryOperation String
gOperationCASSSolComplete =
    generateOperationAnalysisWithCASS "solution completeness" analyseSolCompleteWithCASS

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

--- Generator function to create an information generator for versions.
--- The first argument is a description of the generated information
--- and the second argument is the operation, that looks for the information in the package json file.
generateFromPackageJSON :: Show b => String -> (JValue -> b) -> Generator CurryVersion b
generateFromPackageJSON desc selector opts (CurryVersion pkg vsn) = do
    printDetailMessage opts $ "Generating " ++ desc ++ " for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    packageJSON <- readPackageJSON opts pkg vsn
    case parseJSON packageJSON of
        Nothing -> do
            printDetailMessage opts "Failed to parse package.json."
            return Nothing
        Just jv -> do
            printDebugMessage opts $ "json:\n" ++ ppJSON jv
            let res = selector jv
            printDebugMessage opts $ "Result: " ++ show res
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

--- Generator function to get information from an interface.
--- The first three arguments are the package, the version and the module.
--- The fourth argument is a description of the generated information.
--- The fifth argument is the operation, that looks for the information in the interface of the module.
generateFromInterface :: Show b => Package -> Version -> Module -> String -> (Interface -> Maybe b) -> Options -> IO (Maybe b)
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
generateReference :: SourceCode a => (Options -> a -> IO (Maybe Reference)) -> Generator a Reference
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

--- Generator function to create an information generator using CASS for analysis.
--- The first argument is a description of the analysis
--- and the second argument is the name of the analysis given to CASS as an argument.
generateOperationAnalysisWithCASS :: Show b => String -> (Options -> Package -> Version -> Module -> Operation -> IO (Maybe b)) -> Generator CurryOperation b
generateOperationAnalysisWithCASS desc analysis opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating " ++ desc ++ " analysis of operation '" ++
        o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++
        "' of package '" ++ pkg ++ "'..."
    mres <- analysis opts pkg vsn m o
    processAnalysisResult opts mres

--- This action prints messages about the result depending on the current options.
--- It returns the result as a Maybe value.
finishResult :: Options -> String -> IO (Maybe String)
finishResult opts res = do
    printDebugMessage opts $ "Result is: " ++ res
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

--- This action prints messages about the result depending on the current options and
--- whether a result even exists.
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

getCategories :: JValue -> Maybe [String]
getCategories jv = lookupField "category" jv >>= fromJSONList

getExportedModules :: JValue -> Maybe [String]
getExportedModules jv = lookupField "exportedModules" jv >>= fromJSONList

getDependencies :: JValue -> Maybe [Dependency]
getDependencies jv = case jv of
    JObject fields -> do
        value <- lookup "dependencies" fields
        case value of
            JObject fields' -> do
                mapM convertDependency fields'
            _ -> Nothing
    _ -> Nothing

convertDependency :: (String, JValue) -> Maybe Dependency
convertDependency (pkg, jv) = do
    vcs <- fromJSON jv
    disj <- parseVersionConstraints vcs
    return (Dependency pkg disj)

readPackageModules :: Options -> Package -> Version -> IO [Module]
readPackageModules opts pkg vsn = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return []
        Just dir -> do
            let src = dir </> "src"
            curryModulesInDirectory src

readPackageJSON :: Options -> Package -> Version -> IO String
readPackageJSON opts pkg vsn = do
    printDetailMessage opts "Reading package json..."
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return "{}"
        Just dir -> do
            let packageJSON = dir </> "package.json"
            b <- doesFileExist packageJSON
            case b of
                False -> do
                    return "{}"
                True -> do
                    readFile packageJSON

packageREADMEPath :: Options -> Package -> Version -> IO String
packageREADMEPath opts pkg vsn = do
    printDetailMessage opts "Finding path to README..."
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return ""
        Just dir -> do
            let readme = dir </> "README.md"
            b <- doesFileExist readme
            case b of
                False -> do
                    return ""
                True -> do
                    return readme
