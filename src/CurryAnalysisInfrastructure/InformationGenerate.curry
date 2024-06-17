module CurryAnalysisInfrastructure.InformationGenerate where

import CurryAnalysisInfrastructure.InformationRead (readIndexJSON, readInstalledPackageREADME, readPackage, readVersion)
import CurryAnalysisInfrastructure.JPretty (json)
import CurryAnalysisInfrastructure.JParser (jparse, lookupField, getString)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import System.Directory
import JSON.Pretty (ppJSON)
import JSON.Data

import Text.Pretty (Doc, empty, text)

getInfos :: Maybe [a] -> [a]
getInfos = maybe [] id

findAndReplace :: (a -> Bool) -> a -> [a] -> [a]
findAndReplace _ x [] = [x]
findAndReplace check x (y:ys) 
    | check y = x:ys
    | otherwise = y : findAndReplace check x ys

-- Package

overwritePackageName :: String -> [PackageInformation] -> [PackageInformation]
overwritePackageName name = findAndReplace isPackageName (PackageName name)

overwritePackageVersions :: [String] -> [PackageInformation] -> [PackageInformation]
overwritePackageVersions vsns = findAndReplace isPackageVersions (PackageVersions vsns)

generatePackageName :: String -> IO ()
generatePackageName pkg = do
    -- Read pkg.json
    result <- readPackage pkg
    let pkginfos = getInfos result

    -- Add or overwrite old value with generated value
    let jtext = (ppJSON . json) (overwritePackageName pkg pkginfos)

    -- Write updated information back to json file
    filename <- getPackageFilePath pkg
    writeFile filename jtext

generatePackageVersions :: String -> IO ()
generatePackageVersions pkg = do
    -- Read pkg.json
    result <- readPackage pkg 
    let pkginfos = getInfos result

    -- Get information from index
    i <- index
    let packageDir = i ++ pkg ++ "/"
    contents <- getReducedDirectoryContents packageDir

    -- Add or overwrite old value with generated value
    let jtext = (ppJSON . json) (overwritePackageVersions contents pkginfos)
    
    -- Write updated information back to json file
    filename <- getPackageFilePath pkg
    writeFile filename jtext

-- VERSION

overwriteVersionVersion :: String -> [VersionInformation] -> [VersionInformation]
overwriteVersionVersion vsn = findAndReplace isVersionVersion (VersionVersion vsn)

overwriteVersionDocumentation :: Doc -> [VersionInformation] -> [VersionInformation]
overwriteVersionDocumentation doc = findAndReplace isVersionDocumentation (VersionDocumentation doc)

overwriteVersionCategories :: [String] -> [VersionInformation] -> [VersionInformation]
overwriteVersionCategories cats = findAndReplace isVersionCategories (VersionCategories cats)

overwriteVersionModules :: [String] -> [VersionInformation] -> [VersionInformation]
overwriteVersionModules mods = findAndReplace isVersionModules (VersionModules mods)

generateVersionVersion :: String -> String -> IO ()
generateVersionVersion pkg vsn = do
    -- Read vsn.json
    result <- readVersion pkg vsn
    let vsninfos = getInfos result

    -- Add or overwrite old value with generated value
    let jtext = (ppJSON . json) (overwriteVersionVersion vsn vsninfos)

    -- Write updated information back to json file
    filename <- getVersionFilePath pkg vsn
    writeFile filename jtext

generateVersionDocumentation :: String -> String -> IO ()
generateVersionDocumentation pkg vsn = do
    -- Read vsn.json
    result <- readVersion pkg vsn
    let vsninfos = getInfos result

    -- Get information
    t <- readInstalledPackageREADME pkg vsn
    let doc = text t 

    -- Add or overwrite old value with generated value
    let jtext = (ppJSON . json) (overwriteVersionDocumentation doc vsninfos)

    -- Write updated information back to json file
    filename <- getVersionFilePath pkg vsn
    writeFile filename jtext

generateVersionCategories :: String -> String -> IO ()
generateVersionCategories pkg vsn = do
    -- Read vsn.json
    result <- readVersion pkg vsn
    let vsninfos = getInfos result

    -- Get information
    jvalue <- readIndexJSON pkg vsn
    let cats = maybe [] (\x -> maybe [] id (getCategories x)) jvalue

    -- Add or overwrite old value with generated value
    let jtext = (ppJSON . json) (overwriteVersionCategories cats vsninfos)

    -- Write updated information back to json file
    filename <- getVersionFilePath pkg vsn
    writeFile filename jtext

generateVersionModules :: String -> String -> IO ()
generateVersionModules pkg vsn = do
    -- Read vsn.json
    result <- readVersion pkg vsn
    let vsninfos = getInfos result

    -- Get information
    jvalue <- readIndexJSON pkg vsn
    let mods = maybe [] (\x -> maybe [] id (getExportedModules x)) jvalue

    -- Add or overwrite old value with generated value
    let jtext = (ppJSON . json) (overwriteVersionModules mods vsninfos)

    -- Write updated information back to json file
    filename <- getVersionFilePath pkg vsn
    writeFile filename jtext

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