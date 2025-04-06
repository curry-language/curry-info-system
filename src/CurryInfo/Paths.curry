------------------------------------------------------------------------------
--- This modules defines operations to get paths to files in the local cache.
------------------------------------------------------------------------------

module CurryInfo.Paths where

import Control.Monad    ( unless )
import Data.List        ( isPrefixOf, isSuffixOf )
import Numeric          ( readHex )

import JSON.Pretty      ( ppJSON )
import JSON.Data
import System.Directory ( createDirectoryIfMissing, getDirectoryContents
                        , getHomeDirectory, doesFileExist )
import System.FilePath  ( (</>), (<.>), isAbsolute, joinPath, splitDirectories )

import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Verbosity ( printDebugMessage)

--- Returns the directory path where information about an object is stored.
objectDirectory :: Options -> QueryObject -> FilePath
objectDirectory opts (QueryPackage pkg) = packagesPath opts </> pkg
objectDirectory opts (QueryVersion pkg vsn) =
  objectDirectory opts (QueryPackage pkg) </> "versions" </> vsn
objectDirectory opts (QueryModule pkg vsn m) =
  objectDirectory opts (QueryVersion pkg vsn) </> "modules" </> m
objectDirectory opts (QueryType pkg vsn m _) =
  objectDirectory opts (QueryModule pkg vsn m) </> "types"
objectDirectory opts (QueryClass pkg vsn m _) =
  objectDirectory opts (QueryModule pkg vsn m) </> "classes"
objectDirectory opts (QueryOperation pkg vsn m _) = do
  objectDirectory opts (QueryModule pkg vsn m) </> "operations"

--- Returns the file where information about a request of all operations
--- in a package/version/module are store.
allOperationsReqFile :: Options -> Package -> Version -> Module -> String
                     -> FilePath
allOperationsReqFile opts pkg vsn mn req =
  objectDirectory opts (QueryOperation pkg vsn mn "") </>
  "ALL_" ++ req <.> ".txt"

--- Gets the path of the JSON file containing all information about an object.
objectJSONPath :: Options -> QueryObject -> FilePath
objectJSONPath opts qo =
  let path = objectDirectory opts qo in
  case qo of
    QueryPackage pkg       -> path </> pkg <.> "json"
    QueryVersion _ vsn     -> path </> vsn <.> "json"
    QueryModule _ _ m      -> path </> m <.> "json"
    QueryType _ _ _ t      -> path </> encodeFilePath t <.> "json"
    QueryClass _ _ _ c     -> path </> encodeFilePath c <.> "json"
    QueryOperation _ _ _ o -> path </> encodeFilePath o <.> "json"

--- Encode a file path if it contains special characters with a hexadecimal
--- string encoding.
encodeFilePath :: String -> String
encodeFilePath s = if isSimpleID s then s
                                   else '_': concatMap (intToHex . ord) s
 where
  isSimpleID []     = False
  isSimpleID (x:xs) = isAlpha x && all (\c -> isAlphaNum c || c `elem` ".'_") xs

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
getCPMIndex :: IO FilePath
getCPMIndex = fmap (</> ".cpm" </> "index") getHomeDirectory

--- Transform a relative file path into an absolute path by adding
--- the `optCacheRoot` path.
addRootPath :: Options -> FilePath -> FilePath
addRootPath opts f =
  if isAbsolute f then f
                  else optCacheRoot opts </> f

--- Transform an absolute file path into a relative path by removing
--- a leading `optCacheRoot` path, if possible.
stripRootPath :: Options -> FilePath -> FilePath
stripRootPath opts f = do
  let rdirs = splitDirectories (optCacheRoot opts)
      fdirs = splitDirectories f
  if rdirs `isPrefixOf` fdirs then joinPath (drop (length rdirs) fdirs)
                              else f

--- This action returns the path to the packages directory where
--- the tool stores all information in form of JSON files.
packagesPath :: Options -> FilePath
packagesPath opts = optCacheRoot opts </> "packages"

--- The name of the field identifying re-exported names by their
--- fully qualified name.
realNameField :: String
realNameField = "_realname_"
