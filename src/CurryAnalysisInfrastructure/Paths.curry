module CurryAnalysisInfrastructure.Paths where

import CurryAnalysisInfrastructure.Types

import System.Directory (createDirectoryIfMissing, getDirectoryContents, getHomeDirectory, doesFileExist)

initialize :: Path a => a -> IO ()
initialize x = do
    dir <- getDirectoryPath x
    createDirectoryIfMissing True dir

    jfile <- getJSONPath x
    b <- doesFileExist jfile
    case b of
        False -> writeFile jfile "{}"
        True -> return ()

class Path a where
    getDirectoryPath :: a -> IO String
    getJSONPath :: a -> IO String

instance Path CurryPackage where
    getDirectoryPath (CurryPackage pkg) = do
        path <- root
        return (path ++ "packages/" ++ pkg ++ "/")
    
    getJSONPath x@(CurryPackage pkg) = do
        path <- getDirectoryPath x
        return (path ++ pkg ++ ".json")

instance Path CurryVersion where
    getDirectoryPath (CurryVersion pkg vsn) = do
        path <- getDirectoryPath (CurryPackage pkg)
        return (path ++ "versions/" ++ vsn ++ "/")
    
    getJSONPath x@(CurryVersion _ vsn) = do
        path <- getDirectoryPath x
        return (path ++ vsn ++ ".json")

instance Path CurryModule where
    getDirectoryPath (CurryModule pkg vsn m) = do
        path <- getDirectoryPath (CurryVersion pkg vsn)
        return (path ++ "modules/" ++ m ++ "/")
    
    getJSONPath x@(CurryModule _ _ m) = do
        path <- getDirectoryPath x
        return (path ++ m ++ ".json")

instance Path CurryType where 
    getDirectoryPath (CurryType pkg vsn m t) = do
        path <- getDirectoryPath (CurryModule pkg vsn m)
        return (path ++ "types/" ++ t ++ "/")
    
    getJSONPath x@(CurryType _ _ _ t) = do
        path <- getDirectoryPath x
        return (path ++ t ++ ".json")

instance Path CurryTypeclass where 
    getDirectoryPath (CurryTypeclass pkg vsn m t) = do
        path <- getDirectoryPath (CurryModule pkg vsn m)
        return (path ++ "typeclasses/" ++ t ++ "/")
    
    getJSONPath x@(CurryTypeclass _ _ _ t) = do
        path <- getDirectoryPath x
        return (path ++ t ++ ".json")

instance Path CurryOperation where 
    getDirectoryPath (CurryOperation pkg vsn m op) = do
        path <- getDirectoryPath (CurryModule pkg vsn m)
        return (path ++ "operations/" ++ op ++ "/")
    
    getJSONPath x@(CurryOperation _ _ _ op) = do
        path <- getDirectoryPath x
        return (path ++ op ++ ".json")    

toCheckout :: Package -> Version -> String
toCheckout pkg vsn = pkg ++ "-" ++ vsn

moduleToPath :: Module -> String
moduleToPath = map (\c -> if c == '.' then '/' else c)

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

checkouts :: IO String
checkouts = do
    path <- root
    return (path ++ "checkouts/")

getCheckoutPath :: Package -> Version -> IO String
getCheckoutPath pkg vsn = do
    path <- checkouts
    return (path ++ toCheckout pkg vsn)

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

getModuleFilePath :: String -> String -> String -> IO String
getModuleFilePath pkg vsn m = do
    path <- modulesPath pkg vsn
    return (path ++ m ++ "/" ++ m ++ ".json")