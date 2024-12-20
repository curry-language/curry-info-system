-----------------------------------------------------------------------------------------
--- This modules defines operations to get paths to files in the local cache and to initialize it.
-----------------------------------------------------------------------------------------

module CurryInfo.Paths where

import Data.List        ( isSuffixOf )
import Numeric          ( readHex )
import CurryInfo.Types

import System.Directory ( createDirectoryIfMissing, getDirectoryContents
                        , getHomeDirectory, doesFileExist )
import System.FilePath  ( (</>), (<.>) )

-- This action initializes the directory and json file for the given data.
-- If the json file does not exist, it will be initialized with "{}".
-- If the json already exists, it will remain unchanged.
initializeStore :: QueryObject -> IO ()
initializeStore x = do
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

--- Gets the directory path where an object is stored.
getDirectoryPath :: QueryObject -> IO String
getDirectoryPath (QueryPackage pkg) = do
  path <- packagesPath
  return (path </> pkg)
getDirectoryPath (QueryVersion pkg vsn) = do
  path <- getDirectoryPath (QueryPackage pkg)
  return (path </> "versions" </> vsn)
getDirectoryPath (QueryModule pkg vsn m) = do
  path <- getDirectoryPath (QueryVersion pkg vsn)
  return (path </> "modules" </> m)
getDirectoryPath (QueryType pkg vsn m _) = do
  path <- getDirectoryPath (QueryModule pkg vsn m)
  return (path </> "types")
getDirectoryPath (QueryClass pkg vsn m _) = do
  path <- getDirectoryPath (QueryModule pkg vsn m)
  return (path </> "classes")
getDirectoryPath (QueryOperation pkg vsn m _) = do
  path <- getDirectoryPath (QueryModule pkg vsn m)
  return (path </> "operations")

--- Gets the path of the JSON file containing all information about an object.
getJSONPath :: QueryObject -> IO String
getJSONPath x@(QueryPackage pkg) = do
  path <- getDirectoryPath x
  return (path </> pkg <.> "json")
getJSONPath x@(QueryVersion _ vsn) = do
  path <- getDirectoryPath x
  return (path </> vsn <.> "json")
getJSONPath x@(QueryModule _ _ m) = do
  path <- getDirectoryPath x
  return (path </> m <.> "json")
getJSONPath x@(QueryType _ _ _ t) = do
  path <- getDirectoryPath x
  return (path </> t <.> "json")
getJSONPath x@(QueryClass _ _ _ c) = do
  path <- getDirectoryPath x
  return (path </> c <.> "json")
getJSONPath x@(QueryOperation _ _ _ o) = do
  path <- getDirectoryPath x
  let name = if isOperator o then convert o else o
  return (path </> name <.> "json")
  where
  isOperator = not . and . map isAlphaNum
  convert o' = '_': concat (map (intToHex . ord) o')
  intToHex i = reverse $ map (cs !!)
                              (map (flip mod 16)
                                   (takeWhile (> 0)
                                              (iterate (flip div 16) i)))
  cs = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'A', 'B', 'C', 'D', 'E', 'F']

--- Translates a JSON file name (without directory but with suffix `.json`)
--- into name of corresponding entity.
jsonFile2Name :: String -> Maybe String
jsonFile2Name fns
  | ".json" `isSuffixOf` fns
  = let fn = take (length fns - 5) fns
    in if take 1 fn == "_"
         then decodeHex "" (tail fn)
         else Just fn
  | otherwise = Nothing
 where
  decodeHex s []         = Just (reverse s)
  decodeHex _ [_]        = Nothing
  decodeHex s (c1:c2:cs) = case readHex [c1,c2] of
                             [(h,"")] -> decodeHex (chr h : s) cs
                             _        -> Nothing

-- This action returns the content of a given directory excluding "." and "..".
getReducedDirectoryContents :: String -> IO [String]
getReducedDirectoryContents path =
  fmap (filter (\p -> p /= "." && p /= "..")) (getDirectoryContents path)

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
  return (home </> ".curry_info_cache")

--- This action returns the path to the packages directory, where the local cache of the tool is.
packagesPath :: IO String
packagesPath = do
  path <- root
  return (path </> "packages")