-----------------------------------------------------------------------------------------
--- This modules defines the types used by the rest.
-----------------------------------------------------------------------------------------

module CurryInfo.Types where

import CurryInfo.Helper (parenthesize)

import Text.Pretty (Doc)

import JSON.Data (JValue)

import Data.Char (toLower)
import Data.List (intercalate)

-- Options Type

data Options = Options
  { optVerb           :: Int          -- verbosity level
  , optHelp           :: Bool         -- show usage info?
  , optForce          :: Int          -- only extract/generate if necessary/always generate
  , optPackage        :: Maybe String -- the requested package
  , optVersion        :: Maybe String -- the requested version
  , optModule         :: Maybe String -- the requested module
  , optType           :: Maybe String -- the requested type
  , optClass          :: Maybe String -- the requested type class
  , optOperation      :: Maybe String -- the requested operation
  , optOutFormat      :: OutFormat    -- the output format
  , optOutFile        :: String       -- posible file name to store output
  , optClean          :: Bool         -- clean up information
  , optShowAll        :: Bool         -- show all currently available information
  , optServer         :: Bool         -- run the tool in server mode
  , optPort           :: Maybe Int    -- the port used in server mode
  , optAllTypes       :: Bool         -- process requests for all types
  , optAllClasses     :: Bool         -- Process requests for all type classes
  , optAllOperations  :: Bool         -- Process requests for all operations
  }
  deriving Show 

-- OUTPUT TYPE

data Output
  = OutputText String
  | OutputJSON JValue
  | OutputTerm [(String, String)]
  | OutputError String
  deriving Show

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
    _ -> []

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


-- HELPER TYPES

data Infix = Infix | InfixL | InfixR
  deriving (Read, Show)

type Precedence = Int

type Constructor = String

type Operation = String

type Type = String

type Class = String

type Signature = String

type Version = String

type Module = String

type Category = String

type Package = String

type Method = String

data Reference = Reference String Int Int
  deriving (Show, Read)

-- Dependency

data VersionConstraint
    = VExact Version
    | VGt Version
    | VLt Version
    | VGte Version
    | VLte Version
    | VMinCompatible Version
    | VMajCompatible Version
  deriving (Eq, Show, Read)

type Conjunction = [VersionConstraint]

type Disjunction = [Conjunction]

data Dependency = Dependency Package Disjunction
  deriving (Eq, Show, Read)

--------------------------

-- Request Types

type Generator a b = Options -> a -> IO (Maybe b)

type Printer b = Options -> b -> IO String

------------------------------------------------------------------------------
--- The different kinds of objects passed to `CurryInfo.Information.getInfos`
--- in order to query information about Curry entities.
data QueryObject =
    QueryPackage   Package
  | QueryVersion   Package Version
  | QueryModule    Package Version Module
  | QueryType      Package Version Module Type
  | QueryClass     Package Version Module Class
  | QueryOperation Package Version Module Operation

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
  "Request '" ++ req ++ "' could not be found for for operation " ++ o ++
  " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++
  pkg ++ " could not be read."

--- Transforms an object into a string of tuples with the various components.
--- For entities inside a module, only the module name and entitiy name
--- is printed.
object2StringTuple :: QueryObject -> String
object2StringTuple (QueryPackage pkg) = show pkg
object2StringTuple (QueryVersion pkg vsn) =
  parenthesize (intercalate ", " [show pkg, show vsn])
object2StringTuple (QueryModule pkg vsn m) =
  parenthesize (intercalate ", " [show pkg, show vsn, show m])
object2StringTuple (QueryType _ _ m t) =
  parenthesize (intercalate ", " [show m, show t])
object2StringTuple (QueryClass _ _ m c) =
  parenthesize (intercalate ", " [show m, show c])
object2StringTuple (QueryOperation _ _ m o) =
  parenthesize (intercalate ", " [show m, show o])

------------------------------------------------------------------------------