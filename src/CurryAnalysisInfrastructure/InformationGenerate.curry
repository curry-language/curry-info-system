module CurryAnalysisInfrastructure.InformationGenerate where

import CurryAnalysisInfrastructure.InformationRead (readPackage)
import CurryAnalysisInfrastructure.JPretty (json)
import CurryAnalysisInfrastructure.JParser (jparse)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths
import System.Directory
import JSON.Pretty (ppJSON)

getInfos :: Maybe [a] -> [a]
getInfos = maybe [] id

overwritePackageName :: String -> [PackageInformation] -> [PackageInformation]
overwritePackageName name infos = case infos of
    (PackageName _) : xs -> (PackageName name) : xs
    x : xs -> x : overwritePackageName name xs
    [] -> [PackageName name]

overwritePackageVersions :: [String] -> [PackageInformation] -> [PackageInformation]
overwritePackageVersions vsns infos = case infos of
    (PackageVersions _) : xs -> (PackageVersions vsns) : xs
    x : xs -> x : overwritePackageVersions vsns xs
    [] -> [PackageVersions vsns]
    

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
    contents <- getDirectoryContents packageDir

    -- Add or overwrite old value with generated value
    let jtext = (ppJSON . json) (overwritePackageVersions (drop 2 contents) pkginfos)
    
    -- Write updated information back to json file
    filename <- getPackageFilePath pkg
    writeFile filename jtext