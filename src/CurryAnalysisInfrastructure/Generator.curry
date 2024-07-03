module CurryAnalysisInfrastructure.Generator where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import CurryAnalysisInfrastructure.JParser (getString, lookupField)

import Text.Pretty (text)
import JSON.Data
import JSON.Parser (parseJSON)

import System.IOExts (evalCmd)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.CurryPath (curryModulesInDirectory)

import Data.List (isPrefixOf, intersect)

cmdNotFound :: Int
cmdNotFound = 127

cmdNotExecutable :: Int
cmdNotExecutable = 126

cmdSuccess :: Int
cmdSuccess = 0

-- This action creates a checkout for the given version of the given package.
checkoutIfMissing :: Package -> Version -> IO (Maybe String)
checkoutIfMissing pkg vsn = do
    initializeCheckouts
    path <- getCheckoutPath pkg vsn
    b1 <- doesDirectoryExist path
    case b1 of
        True -> return $ Just path
        False -> do
            --"cypm checkout -o DIR PACKAGE VERSION"
            let cmd = "cypm checkout -o " ++ path ++ " " ++ pkg ++ " " ++ vsn
            (exitCode, _, _) <- evalCmd "cypm" ["checkout", "-o", path, pkg, vsn] ""
            case exitCode of
                127 -> do
                    print "Command 'cypm' was not found"
                    return Nothing
                126 -> do
                    print "Command 'cypm' is not executable"
                    return Nothing
                0 -> do
                    b2 <- doesDirectoryExist path
                    case b2 of
                        True -> return $ Just path
                        False -> do
                            print $ "Checkout for " ++ toCheckout pkg vsn ++ " failed"
                            return Nothing
                _ -> do
                    print $ "Checkout for " ++ toCheckout pkg vsn ++ " failed"
                    return Nothing  

type Generator a b = a -> IO (Maybe b)

-- PACKAGE

type PackageGenerator = Generator CurryPackage PackageInformation

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

type VersionGenerator = Generator CurryVersion VersionInformation

generateVersionVersion :: VersionGenerator
generateVersionVersion (CurryVersion _ vsn) = return $ Just $ VersionVersion vsn

generateVersionDocumentation :: VersionGenerator
generateVersionDocumentation (CurryVersion pkg vsn) = do
    t <- readPackageREADME pkg vsn
    let doc = text t 
    return $ Just $ VersionDocumentation doc

generateVersionCategories :: VersionGenerator
generateVersionCategories (CurryVersion pkg vsn) = do
    packageJSON <- readPackageJSON pkg vsn
    let cats = maybe [] id (parseJSON packageJSON >>= getCategories)
    return $ Just $ VersionCategories cats

generateVersionModules :: VersionGenerator
generateVersionModules (CurryVersion pkg vsn) = do
    allMods <- readPackageModules pkg vsn

    packageJSON <- readPackageJSON pkg vsn
    let exportedMods = maybe allMods id (parseJSON packageJSON >>= getExportedModules)

    return $ Just $ VersionModules (intersect allMods exportedMods)

-- MODULE

type ModuleGenerator = Generator CurryModule ModuleInformation

generateModuleName :: ModuleGenerator
generateModuleName (CurryModule _ _ m) = return $ Just $ ModuleName m

generateModuleDocumentation :: ModuleGenerator
generateModuleDocumentation (CurryModule pkg vsn m) = do
    srcContent <- readModuleSourceFile pkg vsn m
    return $ Just $ (ModuleDocumentation . text) $ unlines $ takeWhile ("--" `isPrefixOf`) (lines srcContent)


generateModuleSourceCode :: ModuleGenerator
generateModuleSourceCode (CurryModule pkg vsn m) = do
    srcContent <- readModuleSourceFile pkg vsn m
    return $ Just $ (ModuleSourceCode . text) srcContent

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

readPackageModules :: Package -> Version -> IO [Module]
readPackageModules pkg vsn = do
    result <- checkoutIfMissing pkg vsn
    case result of
        Nothing -> return []
        Just dir -> do
            let src = dir ++ "/src"
            curryModulesInDirectory src

readPackageJSON :: Package -> Version -> IO String
readPackageJSON pkg vsn = do
    result <- checkoutIfMissing pkg vsn
    case result of
        Nothing -> return "{}"
        Just dir -> do
            let packageJSON = dir ++ "/package.json"
            b <- doesFileExist packageJSON
            case b of
                False -> return "{}"
                True -> readFile packageJSON

readPackageREADME :: Package -> Version -> IO String
readPackageREADME pkg vsn = do
    result <- checkoutIfMissing pkg vsn
    case result of
        Nothing -> return ""
        Just dir -> do
            let readme = dir ++ "/README.md"
            b <- doesFileExist readme
            case b of
                False -> return ""
                True -> readFile readme

readModuleSourceFile :: Package -> Version -> Module -> IO String
readModuleSourceFile pkg vsn m = do
    result <- checkoutIfMissing pkg vsn
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