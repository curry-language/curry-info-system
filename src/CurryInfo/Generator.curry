module CurryInfo.Generator where

import CurryInfo.Types
import CurryInfo.Paths
import CurryInfo.JParser (getString)
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
    ( analyseSafeModule
    , analyseDemandness
    , analyseDeterministic
    , analyseIndeterministic
    , analyseSolutionCompleteness
    , analyseTermination
    , analyseTotallyDefined
    )
import CurryInfo.SourceCode (readSourceCode, readDocumentation)
import CurryInfo.Parser (parseVersionConstraints)
import CurryInfo.Verbosity (printLine, printDebugMessage)

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

type PackageGenerator = Generator CurryPackage PackageInformation

gPackageName :: PackageGenerator
gPackageName opts (CurryPackage pkg) = do
    printLine opts
    printDebugMessage opts $ "Generating name for package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ pkg
    let res = PackageName pkg
    return $ Just (fieldName res, res)

gPackageVersions :: PackageGenerator
gPackageVersions opts (CurryPackage pkg) = do
    printLine opts
    printDebugMessage opts $ "Generating versions for package '" ++ pkg ++ "'..."
    printDebugMessage opts "Looking for package directory in index of package manager..."
    i <- index
    let packageDir = i </> pkg
    printDebugMessage opts $ "Directory in index is: " ++ packageDir
    printDebugMessage opts "Reading content of directory..."
    contents <- getReducedDirectoryContents packageDir
    printDebugMessage opts $ "Versions found: " ++ show contents
    let res = PackageVersions contents
    return $ Just (fieldName res, res)

-- VERSION

type VersionGenerator = Generator CurryVersion VersionInformation

gVersionVersion :: VersionGenerator
gVersionVersion opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating version number for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Version number is: " ++ vsn
    let res = VersionVersion vsn
    return $ Just (fieldName res, res)

gVersionDocumentation :: VersionGenerator
gVersionDocumentation opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading README of package..."
    --t <- readPackageREADME opts pkg vsn
    --printDebugMessage opts $ "Documentation is: " ++ t
    --return $ Just $ VersionDocumentation t
    path <- packageREADMEPath opts pkg vsn
    let res = VersionDocumentation path
    return $ Just (fieldName res, res)

gVersionCategories :: VersionGenerator
gVersionCategories opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating categories for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading 'package.json'..."
    packageJSON <- readPackageJSON opts pkg vsn
    let cats = maybe [] id (parseJSON packageJSON >>= getCategories)
    printDebugMessage opts $ "Categories found: " ++ show cats
    let res = VersionCategories cats
    return $ Just (fieldName res, res)

gVersionModules :: VersionGenerator
gVersionModules opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating modules for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading list of all modules..."
    allMods <- readPackageModules opts pkg vsn

    printDebugMessage opts "Reading list of exported modules..."
    packageJSON <- readPackageJSON opts pkg vsn
    let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

    let intersection = intersect allMods exportedMods
    printDebugMessage opts $ "Exported modules: " ++ show intersection

    let res = VersionModules intersection
    return $ Just (fieldName res, res)

gVersionDependencies :: VersionGenerator
gVersionDependencies opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating dependencies for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading 'package.json'..."
    packageJSON <- readPackageJSON opts pkg vsn
    let deps = maybe [] id (parseJSON packageJSON >>= getDependencies)
    printDebugMessage opts $ "Dependencies found: " ++ show deps
    let res = VersionDependencies deps
    return $ Just (fieldName res, res)

-- MODULE

type ModuleGenerator = Generator CurryModule ModuleInformation

gModuleName :: ModuleGenerator
gModuleName opts (CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating name for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ m
    let res = ModuleName m
    return $ Just (fieldName res, res)

gModuleDocumentation :: ModuleGenerator
gModuleDocumentation opts x@(CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = ModuleDocumentation docs
            return $ Just (fieldName res, res)

gModuleSourceCode :: ModuleGenerator
gModuleSourceCode opts x@(CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating source code for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Source code is: " ++ show source
            let res = ModuleSourceCode source
            return $ Just (fieldName res, res)

gModuleSafe :: ModuleGenerator
gModuleSafe opts (CurryModule pkg vsn m) = do
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
                    let res = ModuleSafe result
                    return $ Just (fieldName res, res)
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gModuleExports :: ModuleGenerator
gModuleExports = failed

gModuleTypeclasses :: ModuleGenerator
gModuleTypeclasses opts (CurryModule pkg vsn m) = do
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
            let res = ModuleTypeclasses exportedClasses
            return $ Just (fieldName res, res)

gModuleTypes :: ModuleGenerator
gModuleTypes opts (CurryModule pkg vsn m) = do
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
            let res = ModuleTypes exportedTypes
            return $ Just (fieldName res, res)

gModuleOperations :: ModuleGenerator
gModuleOperations opts (CurryModule pkg vsn m) = do
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
            let res = ModuleOperations operations
            return $ Just (fieldName res, res)

-- TYPE

type TypeGenerator = Generator CurryType TypeInformation

gTypeName :: TypeGenerator
gTypeName opts (CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating name for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ t
    let res = TypeName t
    return $ Just (fieldName res, res)

gTypeDocumentation :: TypeGenerator
gTypeDocumentation opts x@(CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed"
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = TypeDocumentation docs
            return $ Just (fieldName res, res)

gTypeConstructors :: TypeGenerator
gTypeConstructors opts (CurryType pkg vsn m t) = do
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
            let mconstructors = getTypeDecl t (getAllTypes $ getDeclarations interface) >>= getTypeConstructors
            case mconstructors of
                Nothing -> do
                    printDebugMessage opts "Finding constructors failed."
                    return Nothing
                Just constructors -> do
                    printDebugMessage opts $ "Constructors found: " ++ show constructors
                    let res = TypeConstructors constructors
                    return $ Just (fieldName res, res)

gTypeDefinition :: TypeGenerator
gTypeDefinition opts x@(CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating definition for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..." 
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Definition is: " ++ show source
            let res = TypeDefinition source
            return $ Just (fieldName res, res)

-- TYPECLASS

type TypeclassGenerator = Generator CurryTypeclass TypeclassInformation

gTypeclassName :: TypeclassGenerator
gTypeclassName opts (CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating name of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ c
    let res = TypeclassName c
    return $ Just (fieldName res, res)

gTypeclassDocumentation :: TypeclassGenerator
gTypeclassDocumentation opts x@(CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed"
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = TypeclassDocumentation docs
            return $ Just (fieldName res, res)

gTypeclassMethods :: TypeclassGenerator
gTypeclassMethods opts (CurryTypeclass pkg vsn m c) = do
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
            let mmethods = getClassDecl c (getAllClasses $ getDeclarations interface) >>= getClassMethods
            case mmethods of
                Nothing -> do
                    printDebugMessage opts "Finding methods failed."
                    return Nothing
                Just methods -> do
                    let res = TypeclassMethods methods
                    return $ Just (fieldName res, res)


gTypeclassDefinition :: TypeclassGenerator
gTypeclassDefinition opts x@(CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating definition of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Definition is: " ++ show source
            let res = TypeclassDefinition source
            return $ Just (fieldName res, res)

-- OPERATION

type OperationGenerator = Generator CurryOperation OperationInformation

gOperationName :: OperationGenerator
gOperationName opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating name of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ o
    let res = OperationName o
    return $ Just (fieldName res, res)

gOperationDocumentation :: OperationGenerator
gOperationDocumentation opts x@(CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mdocs <- readDocumentation opts x
    case mdocs of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just docs -> do
            printDebugMessage opts $ "Documentation is: " ++ show docs
            let res = OperationDocumentation docs
            return $ Just (fieldName res, res)

gOperationSourceCode :: OperationGenerator
gOperationSourceCode opts x@(CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating source code of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    msource <- readSourceCode opts x
    case msource of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just source -> do
            printDebugMessage opts $ "Source code is: " ++ show source
            let res = OperationSourceCode source
            return $ Just (fieldName res, res)

gOperationSignature :: OperationGenerator
gOperationSignature opts (CurryOperation pkg vsn m o) = do
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
            let msignature = getOperationDecl o (getOperations $ getDeclarations interface) >>= getOperationSignature
            case msignature of
                Nothing -> do
                    printDebugMessage opts "Finding signature failed."
                    return Nothing
                Just signature -> do
                    printDebugMessage opts $ "Signature is: " ++ show signature
                    let res = OperationSignature signature
                    return $ Just (fieldName res, res)

gOperationInfix :: OperationGenerator
gOperationInfix opts (CurryOperation pkg vsn m o) = do
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
            let res = OperationInfix inf
            return $ Just (fieldName res, res)

gOperationPrecedence :: OperationGenerator
gOperationPrecedence opts (CurryOperation pkg vsn m o) = do
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
            let res = OperationPrecedence precedence
            return $ Just (fieldName res, res)

gOperationDeterministic :: OperationGenerator
gOperationDeterministic opts (CurryOperation pkg vsn m o) = do
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
                    let res = OperationDeterministic result
                    return $ Just (fieldName res, res)
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationDemandness :: OperationGenerator
gOperationDemandness opts (CurryOperation pkg vsn m o) = do
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
                    let res = OperationDemandness result
                    return $ Just (fieldName res, res)
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationIndeterministic :: OperationGenerator
gOperationIndeterministic opts (CurryOperation pkg vsn m o) = do
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
                    let res = OperationIndeterministic result
                    return $ Just (fieldName res, res)
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationSolutionCompleteness :: OperationGenerator
gOperationSolutionCompleteness opts (CurryOperation pkg vsn m o) = do
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
                    let res = OperationSolutionCompleteness result
                    return $ Just (fieldName res, res)
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationTermination :: OperationGenerator
gOperationTermination opts (CurryOperation pkg vsn m o) = do
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
                    let res = OperationTermination result
                    return $ Just (fieldName res, res)
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationTotallyDefined :: OperationGenerator
gOperationTotallyDefined opts (CurryOperation pkg vsn m o) = do
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
                    let res = OperationTotallyDefined result
                    return $ Just (fieldName res, res)
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
            let src = dir </> "src"
            curryModulesInDirectory src

readPackageJSON :: Options -> Package -> Version -> IO String
readPackageJSON opts pkg vsn = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return "{}"
        Just dir -> do
            let packageJSON = dir </> "package.json"
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
            let readme = dir </> "README.md"
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
            let readme = dir </> "README.md"
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
            let src = dir </> "src" </> modNameToPath m <.> "curry"
            b <- doesFileExist src
            case b of 
                False -> do
                    print $ "File " ++ src ++ " does not exist"
                    return ""
                True -> readFile src