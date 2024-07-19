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
import CurryAnalysisInfrastructure.Analysis (analyseSafeModule)

import Text.Pretty (text)
import JSON.Data
import JSON.Parser (parseJSON)

import System.IOExts (evalCmd)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.CurryPath (curryModulesInDirectory)

import Data.List (isPrefixOf, intersect, (\\))
import Data.Maybe (catMaybes)

type Generator a b = Options -> a -> IO (Maybe b)

-- PACKAGE

type PackageGenerator = Generator CurryPackage PackageInformation

generatePackageName :: PackageGenerator
generatePackageName opts (CurryPackage pkg) = return $ Just $ PackageName pkg

generatePackageVersions :: PackageGenerator
generatePackageVersions opts (CurryPackage pkg) = do
    -- Get information from index
    i <- index
    let packageDir = i ++ pkg ++ "/"
    contents <- getReducedDirectoryContents packageDir
    return $ Just $ PackageVersions contents

-- VERSION

type VersionGenerator = Generator CurryVersion VersionInformation

generateVersionVersion :: VersionGenerator
generateVersionVersion opts (CurryVersion _ vsn) = return $ Just $ VersionVersion vsn

generateVersionDocumentation :: VersionGenerator
generateVersionDocumentation opts (CurryVersion pkg vsn) = do
    t <- readPackageREADME opts pkg vsn
    let doc = text t 
    return $ Just $ VersionDocumentation doc

generateVersionCategories :: VersionGenerator
generateVersionCategories opts (CurryVersion pkg vsn) = do
    packageJSON <- readPackageJSON opts pkg vsn
    let cats = maybe [] id (parseJSON packageJSON >>= getCategories)
    return $ Just $ VersionCategories cats

generateVersionModules :: VersionGenerator
generateVersionModules opts (CurryVersion pkg vsn) = do
    allMods <- readPackageModules opts pkg vsn

    packageJSON <- readPackageJSON opts pkg vsn
    let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

    return $ Just $ VersionModules (intersect allMods exportedMods)

-- MODULE

type ModuleGenerator = Generator CurryModule ModuleInformation

generateModuleName :: ModuleGenerator
generateModuleName opts (CurryModule _ _ m) = return $ Just $ ModuleName m

generateModuleDocumentation :: ModuleGenerator
generateModuleDocumentation opts (CurryModule pkg vsn m) = do
    srcContent <- readModuleSourceFile opts pkg vsn m
    return $ Just $ (ModuleDocumentation . text) $ unlines $ takeWhile ("--" `isPrefixOf`) (lines srcContent)


generateModuleSourceCode :: ModuleGenerator
generateModuleSourceCode opts (CurryModule pkg vsn m) = do
    srcContent <- readModuleSourceFile opts pkg vsn m
    return $ Just $ (ModuleSourceCode . text) srcContent

generateModuleSafe :: ModuleGenerator
generateModuleSafe opts (CurryModule pkg vsn m) = do
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            mresult <- analyseSafeModule opts path m
            case mresult of
                Just result -> return $ Just $ ModuleSafe result
                Nothing -> do
                    print "generateModuleSafe: analysis failed"
                    return Nothing
        Nothing -> do
            print "generateModuleSafe: checkout failed"
            return Nothing

generateModuleExports :: ModuleGenerator
generateModuleExports = failed

generateModuleTypeclasses :: ModuleGenerator
generateModuleTypeclasses opts (CurryModule pkg vsn m) = do
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            print "generateModuleTypeclasses: readInterface failed"
            return Nothing
        Just interface -> do
            let allClasses = catMaybes $ map getClassName $ getAllClasses interface
            let hiddenClasses = catMaybes $ map getHiddenClassName $ getHiddenClasses interface
            let exportedClasses = allClasses \\ hiddenClasses
            return $ Just $ ModuleTypeclasses exportedClasses

generateModuleTypes :: ModuleGenerator
generateModuleTypes opts (CurryModule pkg vsn m) = do
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            print "generateModuleTypes: readInterface failed"
            return Nothing
        Just interface -> do
            let allTypes = catMaybes $ map getTypeName $ getAllTypes interface
            let hiddenTypes = catMaybes $ map getHiddenTypeName $ getHiddenTypes interface
            let exportedTypes = allTypes \\ hiddenTypes
            return $ Just $ ModuleTypes exportedTypes

generateModuleOperations :: ModuleGenerator
generateModuleOperations opts (CurryModule pkg vsn m) = do
    minterface <- readInterface opts pkg vsn m
    case minterface of
        Nothing -> do
            print "generateModuleOperations: readInterface failed"
            return Nothing
        Just interface -> do
            return $ Just $ ModuleOperations $ catMaybes $ map getOperationName $ getOperations interface

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