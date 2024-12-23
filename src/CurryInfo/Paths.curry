-----------------------------------------------------------------------------------------
--- This modules defines operations to get paths to files in the local cache and to initialize it.
-----------------------------------------------------------------------------------------

module CurryInfo.Paths where

import Data.List        ( isSuffixOf )
import Numeric          ( readHex )

import JSON.Pretty      ( ppJSON )
import JSON.Data
import System.Directory ( createDirectoryIfMissing, getDirectoryContents
                        , getHomeDirectory, doesFileExist )
import System.FilePath  ( (</>), (<.>) )

import CurryInfo.Types
import CurryInfo.Verbosity ( printDebugMessage)

--- This action initializes the directory and JSON file for the given data.
--- If the JSON file does not exist, it will be initialized with an
--- empty object. If the JSON file already exists, it will remain unchanged.
initializeStore :: Options -> QueryObject -> IO ()
initializeStore opts qobj = initializeStoreWith opts qobj []

--- This action initializes the directory and JSON file for the given data.
--- If the JSON file does not exist, it will be initialized with an object
--- containing a field named by `realNameField` with the qualified name.
--- This is used if entities are defined in a some and re-exported from
--- another module.
--- If the JSON file already exists, it will remain unchanged.
initializeStoreWithRealName :: Options -> QueryObject -> Module -> String
                            -> IO ()
initializeStoreWithRealName opts qobj mn en =
  initializeStoreWith opts qobj [(realNameField, JString $ mn ++ "." ++ en)]

--- This action initializes the directory and JSON file for the given data.
--- If the JSON file does not exist, it will be initialized with the fields
--- provided in the second argument.
--- If the JSON already exists, it will remain unchanged.
initializeStoreWith :: Options -> QueryObject -> [(String, JValue)] -> IO ()
initializeStoreWith opts qobj fields = do
  -- Create directory
  dir <- getDirectoryPath qobj
  createDirectoryIfMissing True dir

  -- Find path to json file
  jfile <- getJSONPath qobj

  -- Check whether json file exists
  b <- doesFileExist jfile
  case b of
    -- Initialize new json file with "{}"
    False -> do let json = ppJSON (JObject fields)
                printDebugMessage opts $ "Initializing store of entity " ++
                  quotePrettyObject qobj ++ " with contents:\n" ++ json
                writeFile jfile json
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
getJSONPath qo@(QueryPackage pkg) = do
  path <- getDirectoryPath qo
  return (path </> pkg <.> "json")
getJSONPath qo@(QueryVersion _ vsn) = do
  path <- getDirectoryPath qo
  return (path </> vsn <.> "json")
getJSONPath qo@(QueryModule _ _ m) = do
  path <- getDirectoryPath qo
  return (path </> m <.> "json")
getJSONPath qo@(QueryType _ _ _ t) = do
  path <- getDirectoryPath qo
  return (path </> t <.> "json")
getJSONPath qo@(QueryClass _ _ _ c) = do
  path <- getDirectoryPath qo
  return (path </> c <.> "json")
getJSONPath qo@(QueryOperation _ _ _ o) = do
  path <- getDirectoryPath qo
  let name = if isSimpleID o then o
                             else '_': concatMap (intToHex . ord) o
  return (path </> name <.> "json")
 where
  isSimpleID []     = False
  isSimpleID (x:xs) = isAlpha x && all (\c -> isAlphaNum c || c `elem` "_") xs

  intToHex i = reverse $ map (cs !!)
                              (map (flip mod 16)
                                   (takeWhile (> 0)
                                              (iterate (flip div 16) i)))
   where
    cs = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
          'A', 'B', 'C', 'D', 'E', 'F']

--- Translates a JSON file name (without directory but with suffix `.json`)
--- into name of corresponding entity.
jsonFile2Name :: String -> Maybe String
jsonFile2Name fns
  | ".json" `isSuffixOf` fns
  = let fn = take (length fns - 5) fns
    in if take 1 fn == "_" then decodeHex "" (tail fn)
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

--- This action returns the path to the root of the local cache.
root :: IO String
root = do
  home <- getHomeDirectory
  return (home </> ".curry_info_cache")

--- This action returns the path to the packages directory where
--- the tool stores all information in form of JSON files.
packagesPath :: IO String
packagesPath = do
  path <- root
  return (path </> "packages")

--- The name of the field identifying re-exported names by their
--- fully qualified name.
realNameField :: String
realNameField = "_realname_"
