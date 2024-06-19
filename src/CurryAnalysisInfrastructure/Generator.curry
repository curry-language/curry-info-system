module CurryAnalysisInfrastructure.Generator where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import CurryAnalysisInfrastructure.JParser (getString, lookupField)

import Text.Pretty (text)
import JSON.Data
import JSON.Parser (parseJSON)

-- PACKAGE

type PackageGenerator = String -> IO (Maybe PackageInformation)

generatePackageName :: PackageGenerator
generatePackageName pkg = return $ Just $ PackageName pkg

generatePackageVersions :: PackageGenerator
generatePackageVersions pkg = do
    -- Get information from index
    i <- index
    let packageDir = i ++ pkg ++ "/"
    contents <- getReducedDirectoryContents packageDir
    return $ Just $ PackageVersions contents

-- VERSION

type VersionGenerator = String -> String -> IO (Maybe VersionInformation)

generateVersionVersion :: VersionGenerator
generateVersionVersion _ vsn = return $ Just $ VersionVersion vsn

generateVersionDocumentation :: VersionGenerator
generateVersionDocumentation pkg vsn = do
    -- Get information
    t <- readInstalledPackageREADME pkg vsn
    let doc = text t 
    return $ Just $ VersionDocumentation doc

generateVersionCategories :: VersionGenerator
generateVersionCategories pkg vsn = do
    -- Get information
    jvalue <- readIndexJSON pkg vsn
    let cats = maybe [] (\x -> maybe [] id (getCategories x)) jvalue
    return $ Just $ VersionCategories cats

generateVersionModules :: VersionGenerator
generateVersionModules pkg vsn = do
    -- Get information
    jvalue <- readIndexJSON pkg vsn
    let mods = maybe [] (\x -> maybe [] id (getExportedModules x)) jvalue
    return $ Just $ VersionModules mods

-- MODULE

type ModuleGenerator = String -> String -> String -> IO (Maybe ModuleInformation)

generateModuleName :: ModuleGenerator
generateModuleName = failed

generateModuleDocumentation :: ModuleGenerator
generateModuleDocumentation = failed

generateModuleSourceCode :: ModuleGenerator
generateModuleSourceCode = failed

generateModuleUnsafe :: ModuleGenerator
generateModuleUnsafe = failed

generateModuleExports :: ModuleGenerator
generateModuleExports = failed

generateModuleTypeclasses :: ModuleGenerator
generateModuleTypeclasses = failed

generateModuleTypes :: ModuleGenerator
generateModuleTypes = failed

generateModuleOperations :: ModuleGenerator
generateModuleOperations = failed

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

readIndexJSON :: String -> String -> IO (Maybe JValue)
readIndexJSON pkg vsn = do
    path <- index
    t <- readFile (path ++ pkg ++ "/" ++ vsn ++ "/package.json")
    return (parseJSON t)

readInstalledPackageREADME :: String -> String -> IO String
readInstalledPackageREADME pkg vsn = do
    path <- installedPackagesPath
    t <- readFile (path ++ pkg ++ "-" ++ vsn ++ "/README.md")
    return t