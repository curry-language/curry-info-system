module CurryAnalysisInfrastructure.InformationRead where

import CurryAnalysisInfrastructure.JParser (jparse)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import System.Directory
import JSON.Parser

--- This function looks for a json-file for the given package. If it finds the file, it is read and parsed to get the information out of it.
--- If any field does not match the scheme, Nothing is returned. If the files doesn't exists, Nothing is returned.
readPackage :: String -> IO (Maybe [PackageInformation])
readPackage pkg = do
    filename <- getPackageFilePath pkg
    b <- doesFileExist filename
    if b
        then do
            text <- readFile filename
            case parseJSON text of
                Just jvalue -> return (jparse jvalue)
                Nothing -> return Nothing
        else do
            return Nothing

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