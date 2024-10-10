module CurryInfo.Generator where

import CurryInfo.Types
import CurryInfo.Paths
import CurryInfo.JRead (jrString)
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
import CurryInfo.SourceCode (readSourceCode, readDocumentation)
import CurryInfo.Parser (parseVersionConstraints)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Reader (readInformation)
import CurryInfo.Writer (writeInformation, (<+>))
import CurryInfo.JShow

import Text.Pretty (text)
import JSON.Data
import JSON.Parser (parseJSON)

import System.IOExts (evalCmd)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.CurryPath (curryModulesInDirectory, modNameToPath)
import System.FilePath ((</>), (<.>))

import Data.List (isPrefixOf, intersect, (\\))
import Data.Maybe (catMaybes)

import Control.Monad (when)

import DetParse (parse)

-- PACKAGE

gPackageName :: Generator CurryPackage String
gPackageName opts (CurryPackage pkg) = do
    printDetailMessage opts $ "Generating name for package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ pkg
    let res = pkg
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

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
    let res = vsn
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gVersionDocumentation :: Generator CurryVersion String
gVersionDocumentation opts (CurryVersion pkg vsn) = do
    printDetailMessage opts $ "Generating documentation for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    path <- packageREADMEPath opts pkg vsn
    let res = path
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gVersionCategories :: Generator CurryVersion [String]
gVersionCategories opts (CurryVersion pkg vsn) = do
    printDetailMessage opts $ "Generating categories for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    packageJSON <- readPackageJSON opts pkg vsn
    printDetailMessage opts "Looking for categories..."
    let cats = maybe [] id (parseJSON packageJSON >>= getCategories)
    printDebugMessage opts $ "Categories found: " ++ show cats
    let res = cats
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gVersionModules :: Generator CurryVersion [String]
gVersionModules opts (CurryVersion pkg vsn) = do
    printDetailMessage opts $ "Generating modules for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    allMods <- readPackageModules opts pkg vsn

    packageJSON <- readPackageJSON opts pkg vsn
    let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

    let intersection = intersect allMods exportedMods
    printDebugMessage opts $ "Exported modules: " ++ show intersection

    let res = intersection
    printDetailMessage opts "Generating finished succesfully."
    return $ Just res

gVersionDependencies :: Generator CurryVersion [Dependency]
gVersionDependencies opts (CurryVersion pkg vsn) = do
    printDetailMessage opts $ "Generating dependencies for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    packageJSON <- readPackageJSON opts pkg vsn
    printDetailMessage opts "Looking for dependencies..."
    let deps = maybe [] id (parseJSON packageJSON >>= getDependencies)
    printDebugMessage opts $ "Dependencies found: " ++ show deps
    let res = deps
    printDetailMessage opts "Generating finished succesfully."
    return $ Just res

-- MODULE

gModuleName :: Generator CurryModule String
gModuleName opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating name for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ m
    let res = m
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gModuleDocumentation :: Generator CurryModule Reference
gModuleDocumentation opts x@(CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating documentation for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = docs
            printDetailMessage opts "Generating finished succesfully."
            return $ Just res

gModuleSourceCode :: Generator CurryModule Reference
gModuleSourceCode opts x@(CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating source code for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Source code is: " ++ show source
            let res = source
            printDetailMessage opts "Generating finished succesfully."
            return $ Just res

gModuleUnsafeModule :: Generator CurryModule Unsafe
gModuleUnsafeModule opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating safe analysis for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mresult <- analyseUnsafeModuleWithCASS opts pkg vsn m
    case mresult of
        Just result -> do
            printDebugMessage opts $ "Analysis result: " ++ show result
            let res = result
            printDetailMessage opts "Generating finished succesfully."
            return $ Just res
        Nothing -> do
            printDebugMessage opts "Analysis failed."
            printDetailMessage opts "Generating failed."
            return Nothing

gModuleTypeclasses :: Generator CurryModule [String]
gModuleTypeclasses opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating exported typeclasses for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed"
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading typeclasses from interface..."
            let allClasses = catMaybes $ map getClassName $ getAllClasses $ getDeclarations interface
            let hiddenClasses = catMaybes $ map getHiddenClassName $ getHiddenClasses $ getDeclarations interface
            let exportedClasses = allClasses \\ hiddenClasses
            printDebugMessage opts $ "Typeclasses found: " ++ show exportedClasses
            let res = exportedClasses
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

gModuleTypes :: Generator CurryModule [String]
gModuleTypes opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating exported types for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed"
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading types from interface..."
            let allTypes = catMaybes $ map getTypeName $ getAllTypes $ getDeclarations interface
            let hiddenTypes = catMaybes $ map getHiddenTypeName $ getHiddenTypes $ getDeclarations interface
            let exportedTypes = allTypes \\ hiddenTypes
            printDebugMessage opts $ "Types found: " ++ show exportedTypes
            let res = exportedTypes
            printDetailMessage opts "Generating finished succesfully"
            return $ Just res

gModuleOperations :: Generator CurryModule [String]
gModuleOperations opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating exported operations for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed"
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading operations from interface..."
            let operations = catMaybes $ map getOperationName $ getOperations $ getDeclarations interface
            printDebugMessage opts $ "Operations found: " ++ show operations
            let res = operations
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

-- TYPE

gTypeName :: Generator CurryType String
gTypeName opts (CurryType pkg vsn m t) = do
    printDetailMessage opts $ "Generating name for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ t
    let res = t
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gTypeDocumentation :: Generator CurryType Reference
gTypeDocumentation opts x@(CurryType pkg vsn m t) = do
    printDetailMessage opts $ "Generating documentation for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDetailMessage opts "Generating failed"
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = docs
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

gTypeConstructors :: Generator CurryType [String]
gTypeConstructors opts (CurryType pkg vsn m t) = do
    printDetailMessage opts $ "Generating constructors for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading constructors from interface..."
            let mconstructors = getTypeDecl t (getAllTypes $ getDeclarations interface) >>= getTypeConstructors
            case mconstructors of
                Nothing -> do
                    printDetailMessage opts "Generating failed."
                    return Nothing
                Just constructors -> do
                    printDebugMessage opts $ "Constructors found: " ++ show constructors
                    let res = constructors
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res

gTypeDefinition :: Generator CurryType Reference
gTypeDefinition opts x@(CurryType pkg vsn m t) = do
    printDetailMessage opts $ "Generating definition for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Definition is: " ++ show source
            let res =  source
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

-- TYPECLASS

gTypeclassName :: Generator CurryTypeclass String
gTypeclassName opts (CurryTypeclass pkg vsn m c) = do
    printDetailMessage opts $ "Generating name of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ c
    let res = c
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gTypeclassDocumentation :: Generator CurryTypeclass Reference
gTypeclassDocumentation opts x@(CurryTypeclass pkg vsn m c) = do
    printDetailMessage opts $ "Generating documentation of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDetailMessage opts "Generating failed"
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = docs
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

gTypeclassMethods :: Generator CurryTypeclass [String]
gTypeclassMethods opts (CurryTypeclass pkg vsn m c) = do
    printDetailMessage opts $ "Generating methods of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading methods from interface..."
            let mmethods = getClassDecl c (getAllClasses $ getDeclarations interface) >>= getClassMethods
            case mmethods of
                Nothing -> do
                    printDetailMessage opts "Generating failed."
                    return Nothing
                Just methods -> do
                    let res = methods
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res

gTypeclassDefinition :: Generator CurryTypeclass Reference
gTypeclassDefinition opts x@(CurryTypeclass pkg vsn m c) = do
    printDetailMessage opts $ "Generating definition of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Definition is: " ++ show source
            let res = source
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

-- OPERATION

gOperationName :: Generator CurryOperation String
gOperationName opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating name of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ o
    let res = o
    printDetailMessage opts "Generating finished successfully."
    return $ Just res

gOperationDocumentation :: Generator CurryOperation Reference
gOperationDocumentation opts x@(CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating documentation of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = docs
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

gOperationSourceCode :: Generator CurryOperation Reference
gOperationSourceCode opts x@(CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating source code of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Source code is: " ++ show source
            let res = source
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

gOperationSignature :: Generator CurryOperation Signature
gOperationSignature opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating signature of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading signature from interface..."
            let msignature = getOperationDecl o (getOperations $ getDeclarations interface) >>= getOperationSignature
            case msignature of
                Nothing -> do
                    printDetailMessage opts "Generating failed."
                    return Nothing
                Just signature -> do
                    printDebugMessage opts $ "Signature is: " ++ show signature
                    let res = signature
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res

gOperationInfix :: Generator CurryOperation (Maybe Infix)
gOperationInfix opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating infix of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading infix from interface..."
            let inf = getInfixDecl o (getDeclarations interface) >>= getOperationInfix
            printDebugMessage opts $ "Infix is: " ++ show inf
            let res = inf
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

gOperationPrecedence :: Generator CurryOperation (Maybe Precedence)
gOperationPrecedence opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating precedence of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDetailMessage opts "Reading precedence from interface..."
            let precedence = getInfixDecl o (getDeclarations interface) >>= getOperationPrecedence
            printDebugMessage opts $ "Precedence is: " ++ show precedence
            let res = precedence
            printDetailMessage opts "Generating finished successfully."
            return $ Just res

gOperationCASSDeterministic :: Generator CurryOperation Deterministic
gOperationCASSDeterministic opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating deterministic analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mresult <- analyseDeterministicWithCASS opts pkg vsn m o
    case mresult of
        Just result -> do
            printDebugMessage opts $ "Result is: " ++ show result
            let res = result
            printDetailMessage opts "Generating finished successfully."
            return $ Just res
        Nothing -> do
            printDetailMessage opts "Analysis failed."
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationCASSDemand :: Generator CurryOperation Demand
gOperationCASSDemand opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating demand analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mresult <- analyseDemandWithCASS opts pkg vsn m o
    case mresult of
        Just result -> do
            printDebugMessage opts $ "Result is: " ++ show result
            let res = result
            printDetailMessage opts "Generating finished successfully."
            return $ Just res
        Nothing -> do
            printDetailMessage opts "Analysis failed."
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationCASSIndeterministic :: Generator CurryOperation Indeterministic
gOperationCASSIndeterministic opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating indeterministic analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mresult <- analyseIndeterministicWithCASS opts pkg vsn m o
    case mresult of
        Just result -> do
            printDebugMessage opts $ "Result is: " ++ show result
            let res = result
            printDetailMessage opts "Generating finished successfully."
            return $ Just res
        Nothing -> do
            printDetailMessage opts "Analysis failed."
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationCASSSolComplete :: Generator CurryOperation SolComplete
gOperationCASSSolComplete opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating solution completeness analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mresult <- analyseSolCompleteWithCASS opts pkg vsn m o
    case mresult of
        Just result -> do
            printDebugMessage opts $ "Result is: " ++ show result
            let res = result
            printDetailMessage opts "Generating finished successfully."
            return $ Just res
        Nothing -> do
            printDetailMessage opts "Analysis failed."
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationCASSTerminating :: Generator CurryOperation Terminating
gOperationCASSTerminating opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating terminating analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mresult <- analyseTerminatingWithCASS opts pkg vsn m o
    case mresult of
        Just result -> do
            printDebugMessage opts $ "Result is: " ++ show result
            let res = result
            printDetailMessage opts "Generating finished successfully."
            return $ Just res
        Nothing -> do
            printDetailMessage opts "Analysis failed."
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationCASSTotal :: Generator CurryOperation Total
gOperationCASSTotal =
  createInfoGeneratorWith "totally defined analysis" analyseTotalWithCASS

gOperationFailFree :: Generator CurryOperation String
gOperationFailFree =
  createInfoGeneratorWith "fail-free analysis" analyseFailFree

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
    mresult <- anafun opts pkg vsn m o
    case mresult of
        Just result -> do
            printDebugMessage opts $ "Result: " ++ show result
            printDetailMessage opts "Generating finished successfully."
            return $ Just result
        Nothing -> do
            printDetailMessage opts "Analysis failed."
            printDetailMessage opts "Generating failed."
            return Nothing

-- HELPER

getCategories :: JValue -> Maybe [String]
getCategories jvalue = case jvalue of
    JObject fields -> do
        value <- lookup "category" fields
        case value of
            JArray arr -> sequence $ map jrString arr
            _ -> Nothing
    _ -> Nothing

getExportedModules :: JValue -> Maybe [String]
getExportedModules jvalue = case jvalue of
    JObject fields -> do
        value <- lookup "exportedModules" fields
        case value of
            JArray arr -> sequence $ map jrString arr
            _ -> Nothing
    _ -> Nothing

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
    vcs <- jrString jv
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
