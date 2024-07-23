module CurryAnalysisInfrastructure.Generator where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import CurryAnalysisInfrastructure.JParser (getString, lookupField)
import CurryAnalysisInfrastructure.Checkout (toCheckout, getCheckoutPath, initializeCheckouts, checkoutIfMissing)
import CurryAnalysisInfrastructure.Interface 
    ( readInterface
    , getOperations, getOperationName
    , getAllTypes, getTypeName, getHiddenTypes, getHiddenTypeName
    , getAllClasses, getClassName, getHiddenClasses, getHiddenClassName
    )
import CurryAnalysisInfrastructure.Options
import CurryAnalysisInfrastructure.Analysis
    ( analyseSafeModule
    , analyseDemandness
    , analyseDeterministic
    , analyseIndeterministic
    , analyseSolutionCompleteness
    , analyseTermination
    , analyseTotallyDefined
    )

import Text.Pretty (text)
import JSON.Data
import JSON.Parser (parseJSON)

import System.IOExts (evalCmd)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.CurryPath (curryModulesInDirectory)

import Data.List (isPrefixOf, intersect, (\\))
import Data.Maybe (catMaybes)

import Control.Monad (when)

type Generator a b = Options -> a -> IO (Maybe b)

-- PACKAGE

type PackageGenerator = Generator CurryPackage PackageInformation

generatePackageName :: PackageGenerator
generatePackageName opts (CurryPackage pkg) = do
    when (fullVerbosity opts) (putStrLn $ "Generating name for package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ PackageName pkg

generatePackageVersions :: PackageGenerator
generatePackageVersions opts (CurryPackage pkg) = do
    when (fullVerbosity opts) (putStrLn $ "Generating versions for package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Finding package directory in index of package manager for package " ++ pkg ++ "...")
    i <- index
    let packageDir = i ++ pkg ++ "/"
    when (fullVerbosity opts) (putStrLn $ "Directory in index of package manager for package " ++ pkg ++ ": " ++ packageDir)
    when (fullVerbosity opts) (putStrLn $ "Reading content of directory...")
    contents <- getReducedDirectoryContents packageDir
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ PackageVersions contents

-- VERSION

type VersionGenerator = Generator CurryVersion VersionInformation

generateVersionVersion :: VersionGenerator
generateVersionVersion opts (CurryVersion pkg vsn) = do
    when (fullVerbosity opts) (putStrLn $ "Generating version number for version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Done")
    return $ Just $ VersionVersion vsn

generateVersionDocumentation :: VersionGenerator
generateVersionDocumentation opts (CurryVersion pkg vsn) = do
    when (fullVerbosity opts) (putStrLn $ "Generating documentation for version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Reading README of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    t <- readPackageREADME opts pkg vsn
    let doc = text t
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ VersionDocumentation doc

generateVersionCategories :: VersionGenerator
generateVersionCategories opts (CurryVersion pkg vsn) = do
    when (fullVerbosity opts) (putStrLn $ "Generating categories for version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Reading 'package.json' of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    packageJSON <- readPackageJSON opts pkg vsn
    let cats = maybe [] id (parseJSON packageJSON >>= getCategories)
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ VersionCategories cats

generateVersionModules :: VersionGenerator
generateVersionModules opts (CurryVersion pkg vsn) = do
    when (fullVerbosity opts) (putStrLn $ "Generating modules for version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Reading list of all modules of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    allMods <- readPackageModules opts pkg vsn

    when (fullVerbosity opts) (putStrLn $ "Reading list of exported modules of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    packageJSON <- readPackageJSON opts pkg vsn
    let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ VersionModules (intersect allMods exportedMods)

-- MODULE

type ModuleGenerator = Generator CurryModule ModuleInformation

generateModuleName :: ModuleGenerator
generateModuleName opts (CurryModule pkg vsn m) = do
    when (fullVerbosity opts) (putStrLn $ "Generating name for module " ++ m ++ " of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ ModuleName m

generateModuleDocumentation :: ModuleGenerator
generateModuleDocumentation opts (CurryModule pkg vsn m) = do
    when (fullVerbosity opts) (putStrLn $ "Generating documentation for module " ++ m ++ " of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Reading source file...")
    srcContent <- readModuleSourceFile opts pkg vsn m
    when (fullVerbosity opts) (putStrLn $ "Reading initial comment of source file...")
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ (ModuleDocumentation . text) $ unlines $ takeWhile ("--" `isPrefixOf`) (lines srcContent)


generateModuleSourceCode :: ModuleGenerator
generateModuleSourceCode opts (CurryModule pkg vsn m) = do
    when (fullVerbosity opts) (putStrLn $ "Generating source code for module " ++ m ++ " of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Reading source file...")
    srcContent <- readModuleSourceFile opts pkg vsn m
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ (ModuleSourceCode . text) srcContent

generateModuleSafe :: ModuleGenerator
generateModuleSafe opts (CurryModule pkg vsn m) = do
    when (fullVerbosity opts) (putStrLn $ "Generating analysis for Safe for module " ++ m ++ " of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Checkout if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Starting analysis...")
            mresult <- analyseSafeModule opts path m
            case mresult of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis finished successfully.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return $ Just $ ModuleSafe result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return Nothing
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing

generateModuleExports :: ModuleGenerator
generateModuleExports = failed

generateModuleTypeclasses :: ModuleGenerator
generateModuleTypeclasses opts (CurryModule pkg vsn m) = do
    when (fullVerbosity opts) (putStrLn $ "Generating typeclasses of module " ++ m ++ " of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Start reading interface...")
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Reading interface failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing
        Just interface -> do
            when (fullVerbosity opts) (putStrLn $ "Reading interface successful.")
            when (fullVerbosity opts) (putStrLn $ "Reading typeclasses from interface...")
            let allClasses = catMaybes $ map getClassName $ getAllClasses interface
            let hiddenClasses = catMaybes $ map getHiddenClassName $ getHiddenClasses interface
            let exportedClasses = allClasses \\ hiddenClasses
            when (fullVerbosity opts) (putStrLn $ "Done.")
            return $ Just $ ModuleTypeclasses exportedClasses

generateModuleTypes :: ModuleGenerator
generateModuleTypes opts (CurryModule pkg vsn m) = do
    when (fullVerbosity opts) (putStrLn $ "Generating types of module " ++ m ++ " of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Start reading interface...")
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Reading interface failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing
        Just interface -> do
            when (fullVerbosity opts) (putStrLn $ "Reading interface successful.")
            when (fullVerbosity opts) (putStrLn $ "Reading types from interface...")
            let allTypes = catMaybes $ map getTypeName $ getAllTypes interface
            let hiddenTypes = catMaybes $ map getHiddenTypeName $ getHiddenTypes interface
            let exportedTypes = allTypes \\ hiddenTypes
            when (fullVerbosity opts) (putStrLn $ "Done.")
            return $ Just $ ModuleTypes exportedTypes

generateModuleOperations :: ModuleGenerator
generateModuleOperations opts (CurryModule pkg vsn m) = do
    when (fullVerbosity opts) (putStrLn $ "Generating operations of module " ++ m ++ " of package " ++ pkg ++ " with version " ++ vsn ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Start reading interface...")
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Reading interface failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing
        Just interface -> do
            when (fullVerbosity opts) (putStrLn $ "Reading interface successful.")
            when (fullVerbosity opts) (putStrLn $ "Reading operations from interface...")
            return $ Just $ ModuleOperations $ catMaybes $ map getOperationName $ getOperations interface

-- TYPE

type TypeGenerator = Generator CurryType TypeInformation

generateTypeName :: TypeGenerator
generateTypeName opts (CurryType pkg vsn m t) = do
    when (fullVerbosity opts) (putStrLn $ "Generating name of type " ++ t ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ TypeName t

generateTypeDocumentation :: TypeGenerator
generateTypeDocumentation opts (CurryType pkg vsn m t) = failed

generateTypeConstructors :: TypeGenerator
generateTypeConstructors opts (CurryType pkg vsn m t) = failed

generateTypeDefinition :: TypeGenerator
generateTypeDefinition opts (CurryType pkg vsn m t) = failed

-- TYPECLASS

type TypeclassGenerator = Generator CurryTypeclass TypeclassInformation

generateTypeclassName :: TypeclassGenerator
generateTypeclassName opts (CurryTypeclass pkg vsn m c) = do
    when (fullVerbosity opts) (putStrLn $ "Generating name of typeclass " ++ c ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ TypeclassName c

generateTypeclassDocumentation :: TypeclassGenerator
generateTypeclassDocumentation opts (CurryTypeclass pkg vsn m c) = failed

generateTypeclassMethods :: TypeclassGenerator
generateTypeclassMethods opts (CurryTypeclass pkg vsn m c) = failed

generateTypeclassDefinition :: TypeclassGenerator
generateTypeclassDefinition opts (CurryTypeclass pkg vsn m c) = failed

-- OPERATION

type OperationGenerator = Generator CurryOperation OperationInformation

operationName :: OperationGenerator
operationName opts (CurryOperation pkg vsn m o) = do
    when (fullVerbosity opts) (putStrLn $ "Generating name of operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Done.")
    return $ Just $ OperationName o

operationDocumentation :: OperationGenerator
operationDocumentation opts (CurryOperation pkg vsn m o) = failed

operationSourceCode :: OperationGenerator
operationSourceCode opts (CurryOperation pkg vsn m o) = failed

operationSignature :: OperationGenerator
operationSignature opts (CurryOperation pkg vsn m o) = failed

operationInfix :: OperationGenerator
operationInfix opts (CurryOperation pkg vsn m o) = failed

operationPrecedence :: OperationGenerator
operationPrecedence opts (CurryOperation pkg vsn m o) = failed

operationDeterministic :: OperationGenerator
operationDeterministic opts (CurryOperation pkg vsn m o) = do
    when (fullVerbosity opts) (putStrLn $ "Generating analysis for Determinstic for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Checkout if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Starting analysis...")
            mresult <- analyseDeterministic opts path m o
            case mresult of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis finished successfully.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return $ Just $ OperationDeterministic result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return Nothing
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing

operationDemandness :: OperationGenerator
operationDemandness opts (CurryOperation pkg vsn m o) = do
    when (fullVerbosity opts) (putStrLn $ "Generating analysis for Determinstic for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Checkout if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Starting analysis...")
            mresult <- analyseDemandness opts path m o
            case mresult of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis finished successfully.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return $ Just $ OperationDemandness result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return Nothing
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing

operationIndeterministic :: OperationGenerator
operationIndeterministic opts (CurryOperation pkg vsn m o) = do
    when (fullVerbosity opts) (putStrLn $ "Generating analysis for Determinstic for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Checkout if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Starting analysis...")
            mresult <- analyseIndeterministic opts path m o
            case mresult of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis finished successfully.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return $ Just $ OperationIndeterministic result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return Nothing
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing

operationSolutionCompleteness :: OperationGenerator
operationSolutionCompleteness opts (CurryOperation pkg vsn m o) = do
    when (fullVerbosity opts) (putStrLn $ "Generating analysis for Determinstic for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Checkout if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Starting analysis...")
            mresult <- analyseSolutionCompleteness opts path m o
            case mresult of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis finished successfully.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return $ Just $ OperationSolutionCompleteness result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return Nothing
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing

operationTermination :: OperationGenerator
operationTermination opts (CurryOperation pkg vsn m o) = do
    when (fullVerbosity opts) (putStrLn $ "Generating analysis for Determinstic for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Checkout if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Starting analysis...")
            mresult <- analyseTermination opts path m o
            case mresult of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis finished successfully.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return $ Just $ OperationTermination result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return Nothing
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing

operationTotallyDefined :: OperationGenerator
operationTotallyDefined opts (CurryOperation pkg vsn m o) = do
    when (fullVerbosity opts) (putStrLn $ "Generating analysis for Determinstic for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ "...")
    when (fullVerbosity opts) (putStrLn $ "Checkout if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Starting analysis...")
            mresult <- analyseTotallyDefined opts path m o
            case mresult of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis finished successfully.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return $ Just $ OperationTotallyDefined result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    when (fullVerbosity opts) (putStrLn $ "Done.")
                    return Nothing
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            when (fullVerbosity opts) (putStrLn $ "Generating failed.")
            return Nothing

-- HELPER

getCategories :: JValue -> Maybe [String]
getCategories jvalue = case jvalue of
    JObject fields -> do
        value <- lookupField "category" fields
        case value of
            JArray arr -> sequence $ map getString arr
            _ -> Nothing
    _ -> Nothing

getExportedModules :: JValue -> Maybe [String]
getExportedModules jvalue = case jvalue of
    JObject fields -> do
        value <- lookupField "exportedModules" fields
        case value of
            JArray arr -> sequence $ map getString arr
            _ -> Nothing
    _ -> Nothing

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

readModuleSourceFile :: Options -> Package -> Version -> Module -> IO String
readModuleSourceFile opts pkg vsn m = do
    result <- checkoutIfMissing opts pkg vsn
    case result of
        Nothing -> return ""
        Just dir -> do
            let src = dir ++ "/src/" ++ moduleToPath m ++ ".curry"
            b <- doesFileExist src
            case b of 
                False -> do
                    print $ "File " ++ src ++ " does not exist"
                    return ""
                True -> readFile src