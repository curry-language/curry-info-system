module CurryAnalysisInfrastructure.InformationRead where

import CurryAnalysisInfrastructure.JParser (JParser, jparse)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import System.Directory
import JSON.Data (JValue)
import JSON.Parser

-- PACKAGE

--- This function reads the given json file and returns the parsed result. If the file does not exist, Nothing is returned.
readJSONFile :: JParser a => String -> IO (Maybe [a])
readJSONFile filename = do
    b <- doesFileExist filename
    if b
        then do
            text <- readFile filename
            return (maybe Nothing jparse (parseJSON text))
        else do
            return Nothing

--- This function looks for a json-file for the given package. If it finds the file, it is read and parsed to get the information out of it.
--- If any field does not match the scheme, Nothing is returned. If the files doesn't exists, Nothing is returned.
readPackage :: String -> IO (Maybe [PackageInformation])
readPackage pkg = do
    filename <- getPackageFilePath pkg
    readJSONFile filename

--- This function looks for the name of a package in a list of information about a package. Only the first instance of such an information is returned.
--- If no name is in the list, Nothing is returned.
getPackageName :: [PackageInformation] -> Maybe PackageInformation
getPackageName pkginfos = case pkginfos of 
    PackageName name : _ -> Just (PackageName name)
    _ : xs -> getPackageName xs
    [] -> Nothing

--- This function looks for the list of versions of a package in a list of information about a package. Only the first instance of such an information is returned.
--- If no list of versions is in the list, Nothing is returned.
getPackageVersions :: [PackageInformation] -> Maybe PackageInformation
getPackageVersions pkginfos = case pkginfos of 
    PackageVersions vsns : _ -> Just (PackageVersions vsns)
    _ : xs -> getPackageVersions xs
    [] -> Nothing

-- VERSION

readVersion :: String -> String -> IO (Maybe [VersionInformation])
readVersion pkg vsn = do
    filename <- getVersionFilePath pkg vsn
    readJSONFile filename

getVersionVersion :: [VersionInformation] -> Maybe VersionInformation
getVersionVersion vsninfos = case vsninfos of
    VersionVersion vsn : _ -> Just (VersionVersion vsn)
    _ : xs -> getVersionVersion xs
    _ -> Nothing

getVersionDocumentation :: [VersionInformation] -> Maybe VersionInformation
getVersionDocumentation vsninfos = case vsninfos of
    VersionDocumentation doc : _ -> Just (VersionDocumentation doc)
    _ : xs -> getVersionVersion xs
    _ -> Nothing

getVersionCategories :: [VersionInformation] -> Maybe VersionInformation
getVersionCategories vsninfos = case vsninfos of
    VersionCategories cats : _ -> Just (VersionCategories cats)
    _ : xs -> getVersionCategories xs
    _ -> Nothing

getVersionModules :: [VersionInformation] -> Maybe VersionInformation
getVersionModules vsninfos = case vsninfos of
    VersionModules mods : _ -> Just (VersionModules mods)
    _ : xs -> getVersionModules xs
    _ -> Nothing

-- HELPER

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