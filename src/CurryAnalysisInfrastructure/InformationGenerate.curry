module CurryAnalysisInfrastructure.InformationGenerate where

import CurryAnalysisInfrastructure.InformationRead (readPackage)
import CurryAnalysisInfrastructure.JPretty (json)
import CurryAnalysisInfrastructure.JParser (jparse)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths (packagesPath, index)
import System.Directory
import JSON.Pretty (ppJSON)

generatePackageName :: String -> IO ()
generatePackageName pkg = do
    result <- readPackage pkg
    let pkginfos = case result of 
            Just pkginfos -> pkginfos
            Nothing -> []
    let jtext = (ppJSON . json) (PackageName pkg : pkginfos)
    path <- packagesPath
    let filename = path ++ pkg ++ "/" ++ pkg ++ ".json"
    writeFile filename jtext

generatePackageVersions :: String -> IO ()
generatePackageVersions pkg = do
    result <- readPackage pkg 
    let pkginfos = case result of 
            Just pkginfos -> pkginfos
            Nothing -> []
    i <- index
    let packageDir = i ++ pkg ++ "/"
    contents <- getDirectoryContents packageDir
    let jtext = (ppJSON . json) (PackageVersions (drop 2 contents) : pkginfos)
    path <- packagesPath
    let filename = path ++ pkg ++ "/" ++ pkg ++ ".json"
    writeFile filename jtext