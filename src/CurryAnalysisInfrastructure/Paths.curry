module CurryAnalysisInfrastructure.Paths where

import CurryAnalysisInfrastructure.Types

import System.Directory (createDirectoryIfMissing, getDirectoryContents, getHomeDirectory, doesFileExist)

-- This action initializes the directory and json file for the given data. If the json file does not exist,
-- it will be initialized with "{}". If the json already exists, it will remain unchanged.
initialize :: Path a => a -> IO ()
initialize x = do
    -- Create directory
    dir <- getDirectoryPath x
    createDirectoryIfMissing True dir

    -- Find path to json file
    jfile <- getJSONPath x

    -- Check whether json file exists
    b <- doesFileExist jfile
    case b of
        -- Initialize new json file with "{}"
        False -> writeFile jfile "{}"
        -- Do nothing
        True -> return ()

class Path x where
    getDirectoryPath :: x -> IO String
    getJSONPath :: x -> IO String

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

-- This function converts a module string to a filesystem path by replacing every '.' with '/'.
moduleToPath :: Module -> String
moduleToPath = map (\c -> if c == '.' then '/' else c)

-- This action returns the content of a given directory excluding "." and "..".
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

-- This functions generates the directory name for a given package and version. It is used for
-- checkout.
toCheckout :: Package -> Version -> String
toCheckout pkg vsn = pkg ++ "-" ++ vsn

-- This actions returns the path to the directory used for checkouts.
checkouts :: IO String
checkouts = do
    path <- root
    return (path ++ "checkouts/")

-- This action returns the path to the directory, in which the checkout of the given version
-- of the given package is stored or will be stored.
getCheckoutPath :: Package -> Version -> IO String
getCheckoutPath pkg vsn = do
    path <- checkouts
    return (path ++ toCheckout pkg vsn)

initializeCheckouts :: IO ()
initializeCheckouts = do
    path <- checkouts
    createDirectoryIfMissing True path