------------------------------------------------------------------------------
--- This modules defines the types used by the rest.
------------------------------------------------------------------------------

module CurryInfo.Types where

import Data.Char (toLower)
import Data.List (intercalate)

import JSON.Data   ( JValue )
import JSON.Pretty ( ppJSON )
import Text.Pretty ( Doc )

import CurryInfo.Helper       ( parenthesize, quote )
import CurryInfo.RequestTypes

------------------------------------------------------------------------------
--- The type of tool options.
data Options = Options
  { optVerb           :: Int          -- verbosity level
  , optHelp           :: Bool         -- show usage info?
  , optShowVersion    :: Bool         -- show version?
  , optForce          :: Int          -- force information generation:
                                      -- 0: no generation
                                      -- 1: only generate when missing
                                      -- 2: always generate
  , optPackage        :: Maybe String -- the requested package
  , optVersion        :: Maybe String -- the requested version
  , optModule         :: Maybe String -- the requested module
  , optType           :: Maybe String -- the requested type
  , optClass          :: Maybe String -- the requested type class
  , optOperation      :: Maybe String -- the requested operation
  , optOutFormat      :: OutFormat    -- the output format
  , optOutAsMap       :: Bool         -- output as map (`--format=CurryMap`)?
  , optOutFile        :: String       -- possible file name to store output
  , optHTMLDir        :: String       -- directory to generate HTML cache copy?
  , optClean          :: Bool         -- clean up information
  , optColor          :: Bool         -- use colors in text output
  , optMarkdown       :: Bool         -- use markdown syntax in text output
  , optShowAll        :: Bool         -- show all currently available infos
  , optCGI            :: Bool         -- run the tool in CGI mode
  , optServer         :: Bool         -- run the tool in server mode
  , optPort           :: Maybe Int    -- the port used in server mode
  , optAllTypes       :: Bool         -- process requests for all types
  , optAllClasses     :: Bool         -- process requests for all type classes
  , optAllOperations  :: Bool         -- process requests for all operations
  , optUpdate         :: Bool         -- update package index (by `cypm update`)
  , optRequests       :: Bool         -- only show the list of all requests
  , optCacheRoot      :: FilePath     -- root of local cache files
  }
  deriving Show 

------------------------------------------------------------------------------
--- The type of output data in various formats.
--- Each element of a list in `OutputTerm` has the object identifier in the
--- first component (e..g., `"base 3.3.0 Prelude", see `showQueryObject`)
--- and a list of request name / value pairs in the second component.
data Output
  = OutputText  String
  | OutputJSON  JValue
  | OutputTerm  CurryOutputTerm
  | OutputError String
  | OutputFile  FilePath  -- output the contents of a file
  deriving Show

--- The type of output terms in `CurryTerm` output format.
--- Each element of the list has the object identifier in the
--- first component (e..g., `"base 3.3.0 Prelude", see `showQueryObject`)
--- and a list of request name / value pairs in the second component.
type CurryOutputTerm = [(String, [(String,String)])]

--- Transform a given output to a string.
getOutputString :: Output -> IO String
getOutputString (OutputText txt)  = return txt
getOutputString (OutputJSON jv)   = return $ ppJSON jv
getOutputString (OutputTerm ts)   = return $ show ts
getOutputString (OutputError err) = return $ "Error: " ++ err
getOutputString (OutputFile fn)   = readFile fn

------------------------------------------------------------------------------
--- The type of output formats.
data OutFormat = OutText | OutJSON | OutTerm
  deriving Eq

instance Show OutFormat where
  show OutText = "Text"
  show OutJSON = "JSON"
  show OutTerm = "CurryTerm"

instance Read OutFormat where
  readsPrec _ s = case map toLower s of
    "text"      -> [(OutText, "")]
    "json"      -> [(OutJSON, "")]
    "curryterm" -> [(OutTerm, "")]
    _           -> []

------------------------------------------------------------------------------
-- Types used to configure the request for various kinds of entities.

data CurryPackage = CurryPackage Package
  deriving (Eq, Show, Read)

data CurryVersion = CurryVersion Package Version
  deriving (Eq, Show, Read)

data CurryModule = CurryModule Package Version Module
  deriving (Eq, Show, Read)

data CurryType = CurryType Package Version Module Type
  deriving (Eq, Show, Read)

data CurryClass = CurryClass Package Version Module Class
  deriving (Eq, Show, Read)

data CurryOperation = CurryOperation Package Version Module Operation
  deriving (Eq, Show, Read)

------------------------------------------------------------------------------
--- This data type represents the different kinds of objects passed to
--- `CurryInfo.Information.getInfos` in order to query information
--- about Curry entities.
data QueryObject =
    QueryPackage   Package
  | QueryVersion   Package Version
  | QueryModule    Package Version Module
  | QueryType      Package Version Module Type
  | QueryClass     Package Version Module Class
  | QueryOperation Package Version Module Operation

--- Is the query object a dummy object, i.e., a type, class, or operation
--- without a name? Dummy objects are used to analyze modules which might have
--- no explicitly exported objects of this kind, since such module might have
--- implicitly generated objects (e.g., class operations).
isDummyObject :: QueryObject -> Bool
isDummyObject qo = case qo of
  QueryType      _ _ _ n -> null n
  QueryClass     _ _ _ n -> null n
  QueryOperation _ _ _ n -> null n
  _                      -> False

--- Sets the name of a dummy object store.
dummyEntityName :: String
dummyEntityName = "_DUMMY_"

--- Sets the name of a dummy object store.
setDummyEntityName :: QueryObject -> QueryObject
setDummyEntityName qo = setEName qo dummyEntityName

--- Sets the entity name in a query object.
setEName :: QueryObject -> String -> QueryObject
setEName (QueryPackage _)         en = QueryPackage en
setEName (QueryVersion p _)       en = QueryVersion p en
setEName (QueryModule p v _)      en = QueryModule p v en
setEName (QueryType      p v m _) en = QueryType p v m en
setEName (QueryClass     p v m _) en = QueryClass p v m en
setEName (QueryOperation p v m _) en = QueryOperation p v m en

--- Sets the module and entity name in a type/class/operation query object.
setModEName :: QueryObject -> Module -> String -> QueryObject
setModEName qobject mn en = case qobject of
  QueryType      p v _ _ -> QueryType p v mn en
  QueryClass     p v _ _ -> QueryClass p v mn en
  QueryOperation p v _ _ -> QueryOperation p v mn en
  _                      -> qobject

--- Transforms a query object into a pretty compact string representation.
--- For instance, this is used for details and debug output.
prettyObject :: QueryObject -> String
prettyObject (QueryPackage pkg)       = pkg
prettyObject (QueryVersion pkg vsn)   = pkg ++ "-" ++ vsn
prettyObject (QueryModule pkg vsn m)  = pkg ++ "-" ++ vsn ++ " / " ++ m
prettyObject (QueryType p v m t)      = p ++ "-" ++ v ++ " / " ++ m ++ "." ++ t
prettyObject (QueryClass p v m c)     = p ++ "-" ++ v ++ " / " ++ m ++ "." ++ c
prettyObject (QueryOperation p v m o) = p ++ "-" ++ v ++ " / " ++ m ++ "." ++ o

--- Transforms a query object into a quoted pretty compact string
--- representation. For instance, this is used for details and debug output.
quotePrettyObject :: QueryObject -> String
quotePrettyObject = quote . prettyObject

--- Error message issued when there is an error reading some object.
errorReadingObject :: QueryObject -> String
errorReadingObject (QueryPackage pkg) =
  "JSON file for package " ++ pkg ++ " could not be read."
errorReadingObject (QueryVersion pkg vsn) =
  "JSON file for version " ++ vsn ++ " of package " ++ pkg ++
  " could not be read."
errorReadingObject (QueryModule pkg vsn m) =
  "JSON file for module " ++ m ++ " of version " ++ vsn ++ " of package " ++
  pkg ++ " could not be read."
errorReadingObject (QueryType pkg vsn m t) =
  "JSON file for type " ++ t ++ " of module " ++ m ++ " of version " ++
  vsn ++ " of package " ++ pkg ++ " could not be read."
errorReadingObject (QueryClass pkg vsn m c) =
  "JSON file for class " ++ c ++ " of module " ++ m ++ " of version " ++
  vsn ++ " of package " ++ pkg ++ " could not be read."
errorReadingObject (QueryOperation pkg vsn m o) =
  "JSON file for operation " ++ o ++ " of module " ++ m ++ " of version " ++
  vsn ++ " of package " ++ pkg ++ " could not be read."

--- Error message issued when there is an error getting some request.
errorRequestObject :: QueryObject -> String -> String
errorRequestObject (QueryPackage pkg) req =
  "Request '" ++ req ++ "' could not be found for package '" ++ pkg ++ "'."
errorRequestObject (QueryVersion pkg vsn) req =
  "Request '" ++ req ++ "' could not be found for version '" ++ vsn ++
  "' of package '" ++ pkg ++ "'."
errorRequestObject (QueryModule pkg vsn m) req =
  "Request '" ++ req ++ "' could not be found for module " ++ m ++
  " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorRequestObject (QueryType pkg vsn m t) req =
  "Request '" ++ req ++ "' could not be found for type " ++ t ++
  " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++
  pkg ++ " could not be read."
errorRequestObject (QueryClass pkg vsn m c) req =
  "Request '" ++ req ++ "' could not be found for class " ++ c ++
  " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++
  pkg ++ " could not be read."
errorRequestObject (QueryOperation pkg vsn m o) req =
  "Request '" ++ req ++ "' could not be found for operation " ++ o ++
  " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++
  pkg ++ " could not be read."

--- Shows a query objects as a list of its components.
--- For entities inside a module, only the module name and entitiy name
--- is shown.
showQueryObject :: QueryObject -> String
showQueryObject (QueryPackage pkg)       = pkg
showQueryObject (QueryVersion pkg vsn)   = unwords [pkg,vsn]
showQueryObject (QueryModule pkg vsn m)  = unwords [pkg, vsn, m]
showQueryObject (QueryType _ _ m t)      = unwords [m, t]
showQueryObject (QueryClass _ _ m c)     = unwords [m, c]
showQueryObject (QueryOperation _ _ m o) = unwords [m, o]

------------------------------------------------------------------------------
