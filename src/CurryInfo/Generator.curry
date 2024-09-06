module CurryInfo.Generator where

import CurryInfo.Types
import CurryInfo.Paths
import CurryInfo.JRead (getString)
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

gPackageName :: Generator CurryPackage String
gPackageName opts (CurryPackage pkg) = do
    printLine opts
    printDebugMessage opts $ "Generating name for package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ pkg
    let res = pkg
    return $ Just res

gPackageVersions :: Generator CurryPackage [String]
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
    let res = contents
    return $ Just res

-- VERSION

gVersionVersion :: Generator CurryVersion String
gVersionVersion opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating version number for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Version number is: " ++ vsn
    let res = vsn
    return $ Just res

gVersionDocumentation :: Generator CurryVersion String
gVersionDocumentation opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating documentation for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading README of package..."
    --t <- readPackageREADME opts pkg vsn
    --printDebugMessage opts $ "Documentation is: " ++ t
    --return $ Just $ VersionDocumentation t
    path <- packageREADMEPath opts pkg vsn
    let res = path
    return $ Just res

gVersionCategories :: Generator CurryVersion [String]
gVersionCategories opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating categories for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading 'package.json'..."
    packageJSON <- readPackageJSON opts pkg vsn
    let cats = maybe [] id (parseJSON packageJSON >>= getCategories)
    printDebugMessage opts $ "Categories found: " ++ show cats
    let res = cats
    return $ Just res

gVersionModules :: Generator CurryVersion [String]
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

    let res = intersection
    return $ Just res

gVersionDependencies :: Generator CurryVersion [Dependency]
gVersionDependencies opts (CurryVersion pkg vsn) = do
    printLine opts
    printDebugMessage opts $ "Generating dependencies for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts "Reading 'package.json'..."
    packageJSON <- readPackageJSON opts pkg vsn
    let deps = maybe [] id (parseJSON packageJSON >>= getDependencies)
    printDebugMessage opts $ "Dependencies found: " ++ show deps
    let res = deps
    return $ Just res

-- MODULE

gModuleName :: Generator CurryModule String
gModuleName opts (CurryModule pkg vsn m) = do
    printLine opts
    printDebugMessage opts $ "Generating name for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ m
    let res = m
    return $ Just res

gModuleDocumentation :: Generator CurryModule Reference
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
            let res = docs
            return $ Just res

gModuleSourceCode :: Generator CurryModule Reference
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
            let res = source
            return $ Just res

gModuleSafe :: Generator CurryModule Safe
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
                    let res = result
                    return $ Just res
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gModuleTypeclasses :: Generator CurryModule [String]
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
            let res = exportedClasses
            return $ Just res

gModuleTypes :: Generator CurryModule [String]
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
            let res = exportedTypes
            return $ Just res

gModuleOperations :: Generator CurryModule [String]
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
            let res = operations
            return $ Just res

-- TYPE

gTypeName :: Generator CurryType String
gTypeName opts (CurryType pkg vsn m t) = do
    printLine opts
    printDebugMessage opts $ "Generating name for type '" ++ t ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ t
    let res = t
    return $ Just res

gTypeDocumentation :: Generator CurryType Reference
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
            let res = docs
            return $ Just res

gTypeConstructors :: Generator CurryType [String]
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
                    let res = constructors
                    return $ Just res

gTypeDefinition :: Generator CurryType Reference
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
            let res =  source
            return $ Just res

-- TYPECLASS

gTypeclassName :: Generator CurryTypeclass String
gTypeclassName opts (CurryTypeclass pkg vsn m c) = do
    printLine opts
    printDebugMessage opts $ "Generating name of typeclass '" ++ c ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ c
    let res = c
    return $ Just res

gTypeclassDocumentation :: Generator CurryTypeclass Reference
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
            let res = docs
            return $ Just res

gTypeclassMethods :: Generator CurryTypeclass [String]
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
                    let res = methods
                    return $ Just res

gTypeclassDefinition :: Generator CurryTypeclass Reference
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
            let res = source
            return $ Just res

-- OPERATION

gOperationName :: Generator CurryOperation String
gOperationName opts (CurryOperation pkg vsn m o) = do
    printLine opts
    printDebugMessage opts $ "Generating name of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    printDebugMessage opts $ "Name is: " ++ o
    let res = o
    return $ Just res

gOperationDocumentation :: Generator CurryOperation Reference
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
            let res = docs
            return $ Just res

gOperationSourceCode :: Generator CurryOperation Reference
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
            let res = source
            return $ Just res

gOperationSignature :: Generator CurryOperation Signature
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
                    let res = signature
                    return $ Just res

gOperationInfix :: Generator CurryOperation (Maybe Infix)
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
            let res = inf
            return $ Just res

gOperationPrecedence :: Generator CurryOperation (Maybe Precedence)
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
            let res = precedence
            return $ Just res

gOperationDeterministic :: Generator CurryOperation Deterministic
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
                    let res = result
                    return $ Just res
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationDemandness :: Generator CurryOperation Demandness
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
                    let res = result
                    return $ Just res
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationIndeterministic :: Generator CurryOperation Indeterministic
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
                    let res = result
                    return $ Just res
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationSolutionCompleteness :: Generator CurryOperation SolutionCompleteness
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
                    let res = result
                    return $ Just res
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationTermination :: Generator CurryOperation Termination
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
                    let res = result
                    return $ Just res
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    return Nothing
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing

gOperationTotallyDefined :: Generator CurryOperation TotallyDefined
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
                    let res = result
                    return $ Just res
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