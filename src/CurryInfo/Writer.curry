------------------------------------------------------------------------------
--- This modules defines operations to initialize and update information
--- in the local cache.
------------------------------------------------------------------------------

module CurryInfo.Writer
  ( initializeObject, initializeObjectWithRealName, updateObjectInformation )
 where

import Control.Monad    ( unless )
import Data.List           ( nubBy )

import JSON.Pretty         ( ppJSON )
import JSON.Data
import System.Directory    ( createDirectoryIfMissing, doesFileExist )

import CurryInfo.Reader    ( readObjectInformation )
import CurryInfo.RequestTypes
import CurryInfo.Paths     ( objectDirectory, objectJSONPath, realNameField )
import CurryInfo.Types
import CurryInfo.Verbosity ( printDebugMessage, printDetailMessage )

--- This action initializes the directory and JSON file for the given data.
--- If the JSON file does not exist, it will be initialized with an
--- empty object. If the JSON file already exists, it will remain unchanged.
initializeObject :: Options -> QueryObject -> IO ()
initializeObject opts qobj = initializeObjectWith opts qobj []

--- This action initializes the directory and JSON file for the given data.
--- If the JSON file does not exist, it will be initialized with an object
--- containing a field named by `realNameField` with the qualified name.
--- This is used if entities are defined in a some and re-exported from
--- another module.
--- If the JSON file already exists, it will remain unchanged.
initializeObjectWithRealName :: Options -> QueryObject -> Module -> String
                            -> IO ()
initializeObjectWithRealName opts qobj mn en =
  initializeObjectWith opts qobj [(realNameField, JString $ mn ++ "." ++ en)]

--- This action initializes the directory and JSON file for the given data.
--- If the JSON file does not exist, it will be initialized with the fields
--- provided in the second argument.
--- If the JSON already exists, it will remain unchanged.
initializeObjectWith :: Options -> QueryObject -> [(String, JValue)] -> IO ()
initializeObjectWith opts qobj fields = do
  createDirectoryIfMissing True (objectDirectory opts qobj)
  let jfile = objectJSONPath opts qobj -- find path to JSON file
  existjson <- doesFileExist jfile  -- check whether JSON file exists
  unless existjson $ do
    -- Initialize new json file with fields
    let json = ppJSON (JObject (toJObject fields))
    printDebugMessage opts $ "Initializing store of entity " ++
      quotePrettyObject qobj ++ " with contents:\n" ++ json ++
      "\ninto file " ++ jfile
    writeFile jfile json

--- This action updates the JSON file of an object with the provided fields.
--- Thus, new fields are added and existing fields are replaced with the
--- new information.
updateObjectInformation :: Options -> QueryObject -> [(String, JValue)] -> IO ()
updateObjectInformation opts obj newfields = do
  let path = objectJSONPath opts obj
  printDebugMessage opts $ "Update object in JSON file " ++ path ++ "..."
  initializeObject opts obj
  mfields <- readObjectInformation opts obj
  case mfields of
    Nothing        -> printDebugMessage opts $ errorReadingObject obj
    Just oldjobj -> do
      let newjobj = foldr (uncurry insertField) oldjobj newfields
      writeFile path (ppJSON (JObject newjobj))
  printDetailMessage opts $ "JSON file " ++ path ++ " updated."
