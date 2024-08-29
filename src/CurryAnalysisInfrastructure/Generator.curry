module CurryAnalysisInfrastructure.Generator where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import CurryAnalysisInfrastructure.JParser (getString)
import CurryAnalysisInfrastructure.Checkout (toCheckout, getCheckoutPath, initializeCheckouts, checkoutIfMissing)
import CurryAnalysisInfrastructure.Interface 
    ( readInterface
    , getDeclarations
    , getOperations, getOperationName, getOperationDecl, getOperationSignature
    , getInfixDecl, getOperationInfix
    , getOperationPrecedence
    , getAllTypes, getTypeName, getHiddenTypes, getHiddenTypeName, getTypeDecl, getTypeConstructors
    , getAllClasses, getClassName, getHiddenClasses, getHiddenClassName, getClassDecl, getClassMethods
    )
import CurryAnalysisInfrastructure.Analysis
    ( analyseSafeModule
    , analyseDemandness
    , analyseDeterministic
    , analyseIndeterministic
    , analyseSolutionCompleteness
    , analyseTermination
    , analyseTotallyDefined
    )
import CurryAnalysisInfrastructure.SourceCode (readSourceCode, readDocumentation)
import CurryAnalysisInfrastructure.Parser (parseVersionConstraints)
import CurryAnalysisInfrastructure.Verbosity (printLine, printDebugMessage)

import Text.Pretty (text)
import JSON.Data
import JSON.Parser (parseJSON)

import System.IOExts (evalCmd)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.CurryPath (curryModulesInDirectory, modNameToPath)

import Data.List (isPrefixOf, intersect, (\\))
import Data.Maybe (catMaybes)

import Control.Monad (when)

import DetParse (parse)

-- PACKAGE

type PackageGenerator = Generator CurryPackage PackageInformation

generatePackageName :: PackageGenerator
generatePackageName opts (CurryPackage pkg) = do
    printLine opts
    printDebugMessage opts $ "Generating name for package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ pkg
    return $ Just $ PackageName pkg

generatePackageVersions :: PackageGenerator
generatePackageVersions opts (CurryPackage pkg) = do
    printLine opts
    printDebugMessage opts $ "Generating versions for package '" ++ pkg ++ "'..."
    printDebugMessage opts "Looking for package directory in index of package manager..."
    i <- index
    let packageDir = i ++ pkg ++ "/"
    printDebugMessage opts $ "Directory in index is: " ++ packageDir
    printDebugMessage opts "Reading content of directory..."
    contents <- getReducedDirectoryContents packageDir
    printDebugMessage opts $ "Versions found: " ++ show contents
    return $ Just $ PackageVersions contents

-- VERSION

type VersionGenerator = Generator CurryVersion VersionInformation

generateVersionVersion :: VersionGenerator
generateVersionVersion opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating version number for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Version number is: " ++ vsn
    return $ Just $ VersionVersion vsn

generateVersionDocumentation :: VersionGenerator
generateVersionDocumentation opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading README of package..."
    --t <- readPackageREADME opts pkg vsn
    --printDebugMessage opts $ "Documentation is: " ++ t
    --return $ Just $ VersionDocumentation t
    path <- packageREADMEPath opts pkg vsn
    return $ Just $ VersionDocumentation path

generateVersionCategories :: VersionGenerator
generateVersionCategories opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating categories for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading 'package.json'..."
    packageJSON <- readPackageJSON opts pkg vsn
    let cats = maybe [] id (parseJSON packageJSON >>= getCategories)
    printDebugMessage opts $ "Categories found: " ++ show cats
    return $ Just $ VersionCategories cats

generateVersionModules :: VersionGenerator
generateVersionModules opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating modules for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading list of all modules..."
    allMods <- readPackageModules opts pkg vsn

    printDebugMessage opts "Reading list of exported modules..."
    packageJSON <- readPackageJSON opts pkg vsn
    let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

    let intersection = intersect allMods exportedMods
    printDebugMessage opts $ "Exported modules: " ++ show intersection

    return $ Just $ VersionModules intersection

generateVersionDependencies :: VersionGenerator
generateVersionDependencies opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating dependencies for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading 'package.json'..."
    packageJSON <- readPackageJSON opts pkg vsn
    let deps = maybe [] id (parseJSON packageJSON >>= getDependencies)
    printDebugMessage opts $ "Dependencies found: " ++ show deps
    return $ Just $ VersionDependencies deps

-- MODULE

type ModuleGenerator = Generator CurryModule ModuleInformation

generateModuleName :: ModuleGenerator
generateModuleName opts (CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating name for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ m
    return $ Just $ ModuleName m

generateModuleDocumentation :: ModuleGenerator
generateModuleDocumentation opts x@(CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            return $ Just $ ModuleDocumentation docs

generateModuleSourceCode :: ModuleGenerator
generateModuleSourceCode opts x@(CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating source code for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Source code is: " ++ show source
            return $ Just $ ModuleSourceCode source

generateModuleSafe :: ModuleGenerator
generateModuleSafe opts (CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating safe analysis for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Starting analysis..."
            mresult <- analyseSafeModule opts path m
            case mresult of
                Just result -> do
                    printDebugMessage opts $ "Analysis result: " ++ show result
                    return $ Just $ ModuleSafe result
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

generateModuleExports :: ModuleGenerator
generateModuleExports = failed

generateModuleTypeclasses :: ModuleGenerator
generateModuleTypeclasses opts (CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating exported typeclasses for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Start reading interface..."    
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed"
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading typeclasses from interface..."
            let allClasses = catMaybes $ map getClassName $ getAllClasses $ getDeclarations interface
            let hiddenClasses = catMaybes $ map getHiddenClassName $ getHiddenClasses $ getDeclarations interface
            let exportedClasses = allClasses \\ hiddenClasses
            printDebugMessage opts $ "Typeclasses found: " ++ show exportedClasses
            return $ Just $ ModuleTypeclasses exportedClasses

generateModuleTypes :: ModuleGenerator
generateModuleTypes opts (CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating exported types for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Start reading interface..."    
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed"
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading types from interface..."
            let allTypes = catMaybes $ map getTypeName $ getAllTypes $ getDeclarations interface
            let hiddenTypes = catMaybes $ map getHiddenTypeName $ getHiddenTypes $ getDeclarations interface
            let exportedTypes = allTypes \\ hiddenTypes
            printDebugMessage opts $ "Types found: " ++ show exportedTypes
            return $ Just $ ModuleTypes exportedTypes

generateModuleOperations :: ModuleGenerator
generateModuleOperations opts (CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating exported operations for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Start reading interface..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed"
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading operations from interface..."
            let operations = catMaybes $ map getOperationName $ getOperations $ getDeclarations interface
            printDebugMessage opts $ "Operations found: " ++ show operations
            return $ Just $ ModuleOperations operations

-- TYPE

type TypeGenerator = Generator CurryType TypeInformation

generateTypeName :: TypeGenerator
generateTypeName opts (CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating name for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ t
    return $ Just $ TypeName t

generateTypeDocumentation :: TypeGenerator
generateTypeDocumentation opts x@(CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed"
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            return $ Just $ TypeDocumentation docs

generateTypeConstructors :: TypeGenerator
generateTypeConstructors opts (CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating constructors for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Start reading interface..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading constructors from interface..."
            let constructors = getTypeDecl t (getAllTypes $ getDeclarations interface) >>= getTypeConstructors
            printDebugMessage opts $ "Constructors found: " ++ show constructors
            return $ TypeConstructors <$> constructors

generateTypeDefinition :: TypeGenerator
generateTypeDefinition opts x@(CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating definition for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Definition is: " ++ show source
            return $ Just $ TypeDefinition source

-- TYPECLASS

type TypeclassGenerator = Generator CurryTypeclass TypeclassInformation

generateTypeclassName :: TypeclassGenerator
generateTypeclassName opts (CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating name of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ c
    return $ Just $ TypeclassName c

generateTypeclassDocumentation :: TypeclassGenerator
generateTypeclassDocumentation opts x@(CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed"
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            return $ Just $ TypeclassDocumentation docs

generateTypeclassMethods :: TypeclassGenerator
generateTypeclassMethods opts (CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating methods of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Start reading interface..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading methods from interface..."
            let methods = getClassDecl c (getAllClasses $ getDeclarations interface) >>= getClassMethods
            return $ TypeclassMethods <$> methods

generateTypeclassDefinition :: TypeclassGenerator
generateTypeclassDefinition opts x@(CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating definition of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Definition is: " ++ show source
            return $ Just $ TypeclassDefinition source

-- OPERATION

type OperationGenerator = Generator CurryOperation OperationInformation

generateOperationName :: OperationGenerator
generateOperationName opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating name of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ o
    return $ Just $ OperationName o

generateOperationDocumentation :: OperationGenerator
generateOperationDocumentation opts x@(CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            return $ Just $ OperationDocumentation docs

generateOperationSourceCode :: OperationGenerator
generateOperationSourceCode opts x@(CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating source code of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Source code is: " ++ show source
            return $ Just $ OperationSourceCode source

generateOperationSignature :: OperationGenerator
generateOperationSignature opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating signature of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading interface..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading signature from interface..."
            let signature = getOperationDecl o (getOperations $ getDeclarations interface) >>= getOperationSignature
            printDebugMessage opts $ "Signature is: " ++ show signature
            return $ OperationSignature <$> signature

generateOperationInfix :: OperationGenerator
generateOperationInfix opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating infix of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading interface..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading infix from interface..."
            let inf = getInfixDecl o (getDeclarations interface) >>= getOperationInfix
            printDebugMessage opts $ "Infix is: " ++ show inf
            return $ Just $ OperationInfix inf

generateOperationPrecedence :: OperationGenerator
generateOperationPrecedence opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating precedence of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading interface..."
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just interface -> do
            printDebugMessage opts "Reading precedence from interface..."
            let precedence = getInfixDecl o (getDeclarations interface) >>= getOperationPrecedence
            printDebugMessage opts $ "Precedence is: " ++ show precedence
            return $ Just $ OperationPrecedence precedence

generateOperationDeterministic :: OperationGenerator
generateOperationDeterministic opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating deterministic analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Starting analysis..."
            mresult <- analyseDeterministic opts path m o
            case mresult of
                Just result -> do
                    printDebugMessage opts $ "Result is: " ++ show result
                    return $ Just $ OperationDeterministic result
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

generateOperationDemandness :: OperationGenerator
generateOperationDemandness opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating demandness analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Starting analysis..."
            mresult <- analyseDemandness opts path m o
            case mresult of
                Just result -> do
                    printDebugMessage opts $ "Result is: " ++ show result
                    return $ Just $ OperationDemandness result
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

generateOperationIndeterministic :: OperationGenerator
generateOperationIndeterministic opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating indeterministic analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Starting analysis..."
            mresult <- analyseIndeterministic opts path m o
            case mresult of
                Just result -> do
                    printDebugMessage opts $ "Result is: " ++ show result
                    return $ Just $ OperationIndeterministic result
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

generateOperationSolutionCompleteness :: OperationGenerator
generateOperationSolutionCompleteness opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating solution completeness analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Starting analysis..."
            mresult <- analyseSolutionCompleteness opts path m o
            case mresult of
                Just result -> do
                    printDebugMessage opts $ "Result is: " ++ show result
                    return $ Just $ OperationSolutionCompleteness result
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

generateOperationTermination :: OperationGenerator
generateOperationTermination opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating termination analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Starting analysis..."
            mresult <- analyseTermination opts path m o
            case mresult of
                Just result -> do
                    printDebugMessage opts $ "Result is: " ++ show result
                    return $ Just $ OperationTermination result
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

generateOperationTotallyDefined :: OperationGenerator
generateOperationTotallyDefined opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating totally defined analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Starting analysis..."
            mresult <- analyseTotallyDefined opts path m o
            case mresult of
                Just result -> do
                    printDebugMessage opts $ "Result is: " ++ show result
                    return $ Just $ OperationTotallyDefined result
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

-- HELPER

getCategories :: JValue -> Maybe [String]
getCategories jvalue = case jvalue of
    JObject fields -> do
        value <- lookup "category" fields
        case value of
            JArray arr -> sequence $ map getString arr
            _ -> Nothing
    _ -> Nothing

getExportedModules :: JValue -> Maybe [String]
getExportedModules jvalue = case jvalue of
    JObject fields -> do
        value <- lookup "exportedModules" fields
        case value of
            JArray arr -> sequence $ map getString arr
            _ -> Nothing
    _ -> Nothing

getDependencies :: JValue -> Maybe [Dependency]
getDependencies jv = case jv of
    JObject fields -> do
        value <- lookup "dependencies" fields
        case value of
            JObject fields -> do
                mapM convertDependency fields
            _ -> Nothing
    _ -> Nothing

{-
convertDependency :: JField -> Maybe Dependency
convertDependency (pkg, jv) = do
    bounds <- getString jv
    (lb, ub) <- parseBounds bounds
    return (pkg, lb, ub)
-}

convertDependency :: JField -> Maybe Dependency
convertDependency (pkg, jv) = do
    vcs <- getString jv
    disj <- parseVersionConstraints vcs
    return (Dependency pkg disj)

readPackageModules :: Options -> Package -> Version -> IO [Module]
readPackageModules opts pkg vsn = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return []
        Just dir -> do
            let src = dir ++ "/src"
            curryModulesInDirectory src

readPackageJSON :: Options -> Package -> Version -> IO String
readPackageJSON opts pkg vsn = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return "{}"
        Just dir -> do
            let packageJSON = dir ++ "/package.json"
            b <- doesFileExist packageJSON
            case b of
                False -> return "{}"
                True -> readFile packageJSON

readPackageREADME :: Options -> Package -> Version -> IO String
readPackageREADME opts pkg vsn = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return ""
        Just dir -> do
            let readme = dir ++ "/README.md"
            b <- doesFileExist readme
            case b of
                False -> return ""
                True -> readFile readme

packageREADMEPath :: Options -> Package -> Version -> IO String
packageREADMEPath opts pkg vsn = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return ""
        Just dir -> do
            let readme = dir ++ "/README.md"
            b <- doesFileExist readme
            case b of
                False -> return ""
                True -> return readme

readModuleSourceFile :: Options -> Package -> Version -> Module -> IO String
readModuleSourceFile opts pkg vsn m = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return ""
        Just dir -> do
            let src = dir ++ "/src/" ++ modNameToPath m ++ ".curry"
            b <- doesFileExist src
            case b of 
                False -> do
                    print $ "File " ++ src ++ " does not exist"
                    return ""
                True -> readFile src