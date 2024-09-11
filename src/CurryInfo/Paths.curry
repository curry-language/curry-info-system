module CurryInfo.Paths where

import CurryInfo.Types

import System.Directory (createDirectoryIfMissing, getDirectoryContents, getHomeDirectory, doesFileExist)
import System.FilePath ((</>), (<.>))

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
        path <- packagesPath
        return (path </> pkg)
    
    getJSONPath x@(CurryPackage pkg) = do
        path <- getDirectoryPath x
        return (path </> pkg <.> "json")

instance Path CurryVersion where
    getDirectoryPath (CurryVersion pkg vsn) = do
        path <- getDirectoryPath (CurryPackage pkg)
        return (path </> "versions" </> vsn)
    
    getJSONPath x@(CurryVersion _ vsn) = do
        path <- getDirectoryPath x
        return (path </> vsn <.> "json")

instance Path CurryModule where
    getDirectoryPath (CurryModule pkg vsn m) = do
        path <- getDirectoryPath (CurryVersion pkg vsn)
        return (path </> "modules" </> m)
    
    getJSONPath x@(CurryModule _ _ m) = do
        path <- getDirectoryPath x
        return (path </> m <.> "json")

instance Path CurryType where 
    getDirectoryPath (CurryType pkg vsn m _) = do
        path <- getDirectoryPath (CurryModule pkg vsn m)
        return (path </> "types")
    
    getJSONPath x@(CurryType _ _ _ t) = do
        path <- getDirectoryPath x
        return (path </> t <.> "json")

instance Path CurryTypeclass where 
    getDirectoryPath (CurryTypeclass pkg vsn m _) = do
        path <- getDirectoryPath (CurryModule pkg vsn m)
        return (path </> "typeclasses")
    
    getJSONPath x@(CurryTypeclass _ _ _ c) = do
        path <- getDirectoryPath x
        return (path </> c <.> "json")

instance Path CurryOperation where 
    getDirectoryPath (CurryOperation pkg vsn m _) = do
        path <- getDirectoryPath (CurryModule pkg vsn m)
        return (path </> "operations")
    
    getJSONPath x@(CurryOperation _ _ _ o) = do
            path <- getDirectoryPath x
            let name = if isOperator o then convert o else o
            return (path </> name <.> "json")
        where
            isOperator = not . and . map isAlphaNum
            convert o' = '_': concat (map (intToHex . ord) o')
            intToHex i = reverse $ map (cs !!) (map (flip mod 16) (takeWhile (> 0) (iterate (flip div 16) i)))
            cs = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

-- This action returns the content of a given directory excluding "." and "..".
getReducedDirectoryContents :: String -> IO [String]
getReducedDirectoryContents path = fmap (filter (\p -> p /= "." && p /= "..")) (getDirectoryContents path)
--getReducedDirectoryContents path = fmap (drop 2) $ getDirectoryContents path

--- This action returns the path to the index of the package manager.
index :: IO String
index = do
    home <- getHomeDirectory
    return (home </> ".cpm" </> "index")

--- This action returns the path to the root of the local cache. All gathered json-files
--- are saved here.
root :: IO String
root = do
    home <- getHomeDirectory
    return (home </> "tmp" </> ".curryanalysis")

packagesPath :: IO String
packagesPath = do
    path <- root
    return (path </> "packages")