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

gModuleSafe :: Generator CurryModule Safe
gModuleSafe opts (CurryModule pkg vsn m) = do
    printDetailMessage opts $ "Generating safe analysis for module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseSafeModule opts path m
            case mresult of
                Just (_, result) -> do
                    printDebugMessage opts $ "Analysis result: " ++ show result
                    let res = result
                    printDetailMessage opts "Generating finished succesfully."
                    return $ Just res
                Nothing -> do
                    printDebugMessage opts "Analysis failed."
                    printDetailMessage opts "Generating failed."
                    return Nothing
        Nothing -> do
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

gOperationDeterministic :: Generator CurryOperation Deterministic
gOperationDeterministic opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating deterministic analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseDeterministic opts path m o
            case mresult of
                Just (results, result) -> do
                    printDebugMessage opts "Writing other results..."
                    mapM (addInformation opts (CurryOperation pkg vsn m) jsOperationDeterministic "deterministic") results
                    printDebugMessage opts $ "Result is: " ++ show result
                    let res = result
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res
                Nothing -> do
                    printDetailMessage opts "Analysis failed."
                    printDetailMessage opts "Generating failed."
                    return Nothing
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationDemandness :: Generator CurryOperation Demandness
gOperationDemandness opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating demandness analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseDemandness opts path m o
            case mresult of
                Just (results, result) -> do
                    printDebugMessage opts "Writing other results..."
                    mapM (addInformation opts (CurryOperation pkg vsn m) jsOperationDemandness "demandness") results
                    printDebugMessage opts $ "Result is: " ++ show result
                    let res = result
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res
                Nothing -> do
                    printDetailMessage opts "Analysis failed."
                    printDetailMessage opts "Generating failed."
                    return Nothing
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationIndeterministic :: Generator CurryOperation Indeterministic
gOperationIndeterministic opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating indeterministic analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseIndeterministic opts path m o
            case mresult of
                Just (results, result) -> do
                    printDebugMessage opts "Writing other results..."
                    mapM (addInformation opts (CurryOperation pkg vsn m) jsOperationIndeterministic "indeterministic") results
                    printDebugMessage opts $ "Result is: " ++ show result
                    let res = result
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res
                Nothing -> do
                    printDetailMessage opts "Analysis failed."
                    printDetailMessage opts "Generating failed."
                    return Nothing
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationSolutionCompleteness :: Generator CurryOperation SolutionCompleteness
gOperationSolutionCompleteness opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating solution completeness analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseSolutionCompleteness opts path m o
            case mresult of
                Just (results, result) -> do
                    printDebugMessage opts "Writing other results..."
                    mapM (addInformation opts (CurryOperation pkg vsn m) jsOperationSolutionCompleteness "solutionCompleteness") results
                    printDebugMessage opts $ "Result is: " ++ show result
                    let res = result
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res
                Nothing -> do
                    printDetailMessage opts "Analysis failed."
                    printDetailMessage opts "Generating failed."
                    return Nothing
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationTermination :: Generator CurryOperation Termination
gOperationTermination opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating termination analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseTermination opts path m o
            case mresult of
                Just (results, result) -> do
                    printDebugMessage opts "Writing other results..."
                    mapM (addInformation opts (CurryOperation pkg vsn m) jsOperationTermination "termination") results
                    printDebugMessage opts $ "Result is: " ++ show result
                    let res = result
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res
                Nothing -> do
                    printDetailMessage opts "Analysis failed."
                    printDetailMessage opts "Generating failed."
                    return Nothing
        Nothing -> do
            printDetailMessage opts "Generating failed."
            return Nothing

gOperationTotallyDefined :: Generator CurryOperation TotallyDefined
gOperationTotallyDefined opts (CurryOperation pkg vsn m o) = do
    printDetailMessage opts $ "Generating totally defined analysis of operation '" ++ o ++ "' of module '" ++ m ++ "' of version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseTotallyDefined opts path m o
            case mresult of
                Just (results, result) -> do
                    printDebugMessage opts "Writing other results..."
                    mapM (addInformation opts (CurryOperation pkg vsn m) jsOperationTotallyDefined "totallyDefined") results
                    printDebugMessage opts $ "Result is: " ++ show result
                    let res = result
                    printDetailMessage opts "Generating finished successfully."
                    return $ Just res
                Nothing -> do
                    printDetailMessage opts "Analysis failed."
                    printDetailMessage opts "Generating failed."
                    return Nothing
        Nothing -> do
            printDetailMessage opts "Generating failed."
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
            JObject fields' -> do
                mapM convertDependency fields'
            _ -> Nothing
    _ -> Nothing

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

readPackageREADME :: Options -> Package -> Version -> IO String
readPackageREADME opts pkg vsn = do
    printDetailMessage opts "Reading README..."
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
                    readFile readme

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

addInformation :: Path a => Options -> (String -> a) -> JShower b -> String -> (String, b) -> IO ()
addInformation opts constructor jshower field (name, result) = do
    let obj = constructor name
    initialize obj
    mfields <- readInformation opts obj
    case mfields of
        Nothing -> do
            return ()
        Just fields -> do
            let newInformation = [(field, jshower result)] <+> fields
            writeInformation obj newInformation
