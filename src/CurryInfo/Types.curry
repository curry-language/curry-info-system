module CurryInfo.Types where

import Text.Pretty (Doc)

import JSON.Data (JValue)

import Data.Char (toLower)

-- Options Type

data Options = Options
    { optVerb       :: Int          -- The verbosity
    , optHelp       :: Bool         -- Usage info
    , optForce      :: Int          -- Only Extract - Generate if necessary - Generate always
    , optPackage    :: Maybe String -- The requested package
    , optVersion    :: Maybe String -- The requested version
    , optModule     :: Maybe String -- The requested module
    , optType       :: Maybe String -- The requested type
    , optTypeclass  :: Maybe String -- The requested type class
    , optOperation  :: Maybe String -- The requested operation
    , optOutput     :: OutFormat    -- The output format
    , optClean      :: Bool         -- Clean up information
    , optShowAll    :: Bool         -- Show all currently available information
    }
    deriving Show 

class Field a where
    fieldName :: a -> String

-- OUTPUT TYPE

data Output
    = OutputText String
    | OutputJSON JValue
    | OutputTerm [(String, String)]
    | OutputError String
    deriving Show

-- INPUT TYPES

data CurryPackage = CurryPackage Package

data CurryVersion = CurryVersion Package Version

data CurryModule = CurryModule Package Version Module

data CurryType = CurryType Package Version Module Type

data CurryTypeclass = CurryTypeclass Package Version Module Typeclass

data CurryOperation = CurryOperation Package Version Module Operation

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

-- JSON

type JField = (String, JValue)

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

type Generator a b = Options -> a -> IO (Maybe b)

type JReader b = JValue -> Maybe b

type JShower b = b -> JValue

type Printer b = Options -> b -> IO String

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