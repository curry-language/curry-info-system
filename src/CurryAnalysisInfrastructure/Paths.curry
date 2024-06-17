module CurryAnalysisInfrastructure.Paths where

import System.Directory

getReducedDirectoryContents :: String -> IO [String]
getReducedDirectoryContents path = fmap (drop 2) $ getDirectoryContents path

--- This action returns the path to the index of the package manager.
index :: IO String
index = do
    home <- getHomeDirectory
    return (home ++ "/.cpm/index/")

--- This action returns the path to the root of the local cache. All gathered json-files
--- are saved here.
root :: IO String
root = do
    home <- getHomeDirectory
    return (home ++ "/tmp" ++ "/.curryanalysis/")

installedPackagesPath :: IO String
installedPackagesPath = do
    home <- getHomeDirectory
    return (home ++ "/.cpm/packages/")

--- This action returns the path where the directories for the packages are saved,
--- for which information has been gathered.
packagesPath :: IO String
packagesPath = do 
    r <- root
    return (r ++ "packages/")

--- This actions returns the path where the versions of a given package are saved.
versionsPath :: String -> IO String
versionsPath pkg = do
    path <- packagesPath
    return (path ++ pkg ++ "/versions/")

--- This actions returns the path where the modules of a version of a package are saved.
modulesPath :: String -> String -> IO String
modulesPath pkg vsn = do
    path <- versionsPath pkg
    return (path ++ vsn ++ "/modules/")

--- This actions returns the path where the types of a module of a version of a package are saved.
typesPath :: String -> String -> String -> IO String
typesPath pkg vsn m = do 
    path <- modulesPath pkg vsn
    return (path ++ m ++ "/types/")

--- This actions returns the path where the typeclasses of a module of a version of a package are saved.
typeclassesPath :: String -> String -> String -> IO String
typeclassesPath pkg vsn m = do 
    path <- modulesPath pkg vsn
    return (path ++ m ++ "/typeclasses/")

--- This actions returns the path where the operations of a module of a version of a package are saved.
operationsPath :: String -> String -> String -> IO String
operationsPath pkg vsn m = do 
    path <- modulesPath pkg vsn
    return (path ++ m ++ "/operations/")

getPackageFilePath :: String -> IO String
getPackageFilePath pkg = do
    path <- packagesPath
    return (path ++ pkg ++ "/" ++ pkg ++ ".json")

getVersionFilePath :: String -> String -> IO String
getVersionFilePath pkg vsn = do
    path <- versionsPath pkg
    return (path ++ vsn ++ "/" ++ vsn ++ ".json")