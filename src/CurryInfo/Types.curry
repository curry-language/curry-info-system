module CurryInfo.Types where

import Text.Pretty (Doc)

import JSON.Data (JValue)

import Data.Char (toLower)

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

-- INPUT TYPES

data CurryPackage = CurryPackage Package

data CurryVersion = CurryVersion Package Version

data CurryModule = CurryModule Package Version Module

data CurryType = CurryType Package Version Module Type

data CurryTypeclass = CurryTypeclass Package Version Module Typeclass

data CurryOperation = CurryOperation Package Version Module Operation

class ErrorMessage a where
    errorReading :: a -> String
    errorRequest :: a -> String -> String

instance ErrorMessage CurryPackage where
    errorReading (CurryPackage pkg) =
        "JSON file for package " ++ pkg ++ " could not be read."

    errorRequest (CurryPackage pkg) req =
        "Request '" ++ req ++ "' could not be found for package '" ++ pkg ++ "'."

instance ErrorMessage CurryVersion where
    errorReading (CurryVersion pkg vsn) =
        "JSON file for version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

    errorRequest (CurryVersion pkg vsn) req =
        "Request '" ++ req ++ "' could not be found for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'."

instance ErrorMessage CurryModule where
    errorReading (CurryModule pkg vsn m) =
        "JSON file for module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

    errorRequest (CurryModule pkg vsn m) req =
        "Request '" ++ req ++ "' could not be found for for module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryType where
    errorReading (CurryType pkg vsn m t) =
        "JSON file for type " ++ t ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

    errorRequest (CurryType pkg vsn m t) req =
        "Request '" ++ req ++ "' could not be found for for type " ++ t ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryTypeclass where
    errorReading (CurryTypeclass pkg vsn m c) =
        "JSON file for typeclass " ++ c ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

    errorRequest (CurryTypeclass pkg vsn m c) req =
        "Request '" ++ req ++ "' could not be found for for typeclass " ++ c ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryOperation where
    errorReading (CurryOperation pkg vsn m o) =
        "JSON file for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

    errorRequest (CurryOperation pkg vsn m o) req =
        "Request '" ++ req ++ "' could not be found for for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

-- HELPER TYPES

data Infix = Infix | InfixL | InfixR
    deriving (Read, Show)

type Precedence = Int

type External = ()

type Constructor = String

type Operation = String

type Type = String

type Typeclass = String

type Export = String

type Signature = String

type Version = String

type Module = String

type Category = String

type Package = String

type Method = String

type Reference = (String, Int, Int)

-- Result Types

data Safe
    = Safe
    | Unsafe
    | UnsafeDue [Module]
    deriving (Show, Read)

data Deterministic
    = Det
    | NDet
    deriving (Show, Read)

type Demandness = [Int]

type Indeterministic = Bool

type SolutionCompleteness = Bool

type Termination = Bool

type TotallyDefined = Bool

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

type JReader b = JValue -> Maybe b

type JShower b = b -> JValue

type Printer b = Options -> b -> IO String
