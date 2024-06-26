module CurryAnalysisInfrastructure.Generator where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import CurryAnalysisInfrastructure.JParser (getString, lookupField)

import Text.Pretty (text)
import JSON.Data
import JSON.Parser (parseJSON)

import System.IOExts (evalCmd)
import System.Directory (doesDirectoryExist)

import Data.List (isPrefixOf)

checkoutIfMissing :: Package -> Version -> IO (Maybe String)
checkoutIfMissing pkg vsn = do
    path <- getCheckoutPath pkg vsn
    b1 <- doesDirectoryExist path
    case b1 of
        True -> return $ Just path
        False -> do
            --"cypm checkout -o DIR PACKAGE VERSION"
            let cmd = "cypm checkout -o " ++ path ++ " " ++ pkg ++ " " ++ vsn
            print cmd
            evalCmd cmd [] ""
            --evalCmd "cypm checkout -o" [path, pkg, vsn] ""

            -- wait for cmd to finish -> HOW?

            b2 <- doesDirectoryExist path
            case b2 of
                True -> return $ Just path
                False -> do
                    print $ "Checkout for " ++ toCheckout pkg vsn ++ " failed"
                    return Nothing

-- PACKAGE

type PackageGenerator = CurryPackage -> IO (Maybe PackageInformation)

generatePackageName :: PackageGenerator
generatePackageName (CurryPackage pkg) = return $ Just $ PackageName pkg

generatePackageVersions :: PackageGenerator
generatePackageVersions (CurryPackage pkg) = do
    -- Get information from index
    i <- index
    let packageDir = i ++ pkg ++ "/"
    contents <- getReducedDirectoryContents packageDir
    return $ Just $ PackageVersions contents

-- VERSION

type VersionGenerator = CurryVersion -> IO (Maybe VersionInformation)

generateVersionVersion :: VersionGenerator
generateVersionVersion (CurryVersion _ vsn) = return $ Just $ VersionVersion vsn

generateVersionDocumentation :: VersionGenerator
generateVersionDocumentation (CurryVersion pkg vsn) = do
    -- Get information
    t <- readInstalledPackageREADME pkg vsn
    let doc = text t 
    return $ Just $ VersionDocumentation doc

generateVersionCategories :: VersionGenerator
generateVersionCategories (CurryVersion pkg vsn) = do
    -- Get information
    jvalue <- readIndexJSON pkg vsn
    let cats = maybe [] (\x -> maybe [] id (getCategories x)) jvalue
    return $ Just $ VersionCategories cats

generateVersionModules :: VersionGenerator
generateVersionModules (CurryVersion pkg vsn) = do
    -- Get information
    jvalue <- readIndexJSON pkg vsn
    let mods = maybe [] (\x -> maybe [] id (getExportedModules x)) jvalue
    return $ Just $ VersionModules mods

-- MODULE

type ModuleGenerator = CurryModule -> IO (Maybe ModuleInformation)

generateModuleName :: ModuleGenerator
generateModuleName (CurryModule _ _ m) = return $ Just $ ModuleName m

generateModuleDocumentation :: ModuleGenerator
generateModuleDocumentation x@(CurryModule pkg vsn m) = do
    result <- checkoutIfMissing pkg vsn
    case result of
        Nothing -> return Nothing
        Just dir -> do
            let src = dir ++ "/src/" ++ moduleToPath m ++ ".curry"
            content <- readFile src
            return $ Just $ (ModuleDocumentation . text) $ unlines $ takeWhile ("--" `isPrefixOf`) (lines content)


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