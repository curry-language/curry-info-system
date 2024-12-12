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
  { optVerb           :: Int          -- The verbosity
  , optHelp           :: Bool         -- Usage info
  , optForce          :: Int          -- Only Extract - Generate if necessary - Generate always
  , optPackage        :: Maybe String -- The requested package
  , optVersion        :: Maybe String -- The requested version
  , optModule         :: Maybe String -- The requested module
  , optType           :: Maybe String -- The requested type
  , optTypeclass      :: Maybe String -- The requested type class
  , optOperation      :: Maybe String -- The requested operation
  , optOutput         :: OutFormat    -- The output format
  , optClean          :: Bool         -- Clean up information
  , optShowAll        :: Bool         -- Show all currently available information
  , optServer         :: Bool         -- Run the tool in server mode
  , optPort           :: Maybe Int    -- The port used in server mode
  , optAllTypes       :: Bool         -- Process requests for all types in given module
  , optAllTypeclasses :: Bool         -- Process requests for all typeclasses in given module
  , optAllOperations  :: Bool         -- Process requests for all operations in given module
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

data CurryTypeclass = CurryTypeclass Package Version Module Typeclass
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

type Typeclass = String

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
--- in order to query some information about Curry entities.
data QueryObject =
  PackageObject   Package
  | VersionObject   Package Version
  | ModuleObject    Package Version Module
  | TypeObject      Package Version Module Type
  | TypeClassObject Package Version Module Typeclass
  | OperationObject Package Version Module Operation

--- Error message issued when there is an error reading some object.
errorReadingObject :: QueryObject -> String
errorReadingObject (PackageObject pkg) =
  "JSON file for package " ++ pkg ++ " could not be read."
errorReadingObject (VersionObject pkg vsn) =
  "JSON file for version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorReadingObject (ModuleObject pkg vsn m) =
  "JSON file for module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorReadingObject (TypeObject pkg vsn m t) =
  "JSON file for type " ++ t ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorReadingObject (TypeClassObject pkg vsn m c) =
  "JSON file for typeclass " ++ c ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorReadingObject (OperationObject pkg vsn m o) =
  "JSON file for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

--- Error message issued when there is an error getting some request.
errorRequestObject :: QueryObject -> String -> String
errorRequestObject (PackageObject pkg) req =
  "Request '" ++ req ++ "' could not be found for package '" ++ pkg ++ "'."
errorRequestObject (VersionObject pkg vsn) req =
  "Request '" ++ req ++ "' could not be found for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'."
errorRequestObject (ModuleObject pkg vsn m) req =
  "Request '" ++ req ++ "' could not be found for module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorRequestObject (TypeObject pkg vsn m t) req =
  "Request '" ++ req ++ "' could not be found for type " ++ t ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorRequestObject (TypeClassObject pkg vsn m c) req =
  "Request '" ++ req ++ "' could not be found for typeclass " ++ c ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."
errorRequestObject (OperationObject pkg vsn m o) req =
  "Request '" ++ req ++ "' could not be found for for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

--- Transforms an object into a string of tuples with the various components.
object2StringTuple :: QueryObject -> String
object2StringTuple (PackageObject pkg) = show pkg
object2StringTuple (VersionObject pkg vsn) =
  parenthesize (intercalate ", " [show pkg, show vsn])
object2StringTuple (ModuleObject pkg vsn m) =
  parenthesize (intercalate ", " [show pkg, show vsn, show m])
object2StringTuple (TypeObject pkg vsn m t) =
  parenthesize (intercalate ", " [show pkg, show vsn, show m, show t])
object2StringTuple (TypeClassObject pkg vsn m c) =
  parenthesize (intercalate ", " [show pkg, show vsn, show m, show c])
object2StringTuple (OperationObject pkg vsn m o) =
  parenthesize (intercalate ", " [show pkg, show vsn, show m, show o])

------------------------------------------------------------------------------