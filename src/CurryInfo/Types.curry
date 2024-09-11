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

-- INFORMATION TYPES

{-
data PackageInformation
    = PackageName String
    | PackageVersions [Version]

data VersionInformation
    = VersionVersion Version
    | VersionDocumentation String
    | VersionCategories [Category]
    | VersionModules [Module]
    | VersionDependencies [Dependency]

data ModuleInformation
    = ModuleName String
    | ModuleDocumentation Reference
    | ModuleSourceCode Reference
    | ModuleSafe Safe
    | ModuleExports [Export]
    | ModuleTypeclasses [Typeclass]
    | ModuleTypes [Type]
    | ModuleOperations [Operation]

data TypeInformation
    = TypeName String
    | TypeDocumentation Reference
    | TypeConstructors [Constructor]
    | TypeDefinition Reference

data TypeclassInformation
    = TypeclassName String
    | TypeclassDocumentation Reference
    | TypeclassMethods [Signature]
    | TypeclassDefinition Reference

data OperationInformation
    = OperationName String
    | OperationDocumentation Reference
    | OperationSourceCode Reference
    | OperationSignature Signature
    | OperationInfix (Maybe Infix)
    | OperationPrecedence (Maybe Precedence)
    | OperationDeterministic Deterministic
    | OperationDemandness Demandness
    | OperationIndeterministic Indeterministic
    | OperationSolutionCompleteness SolutionCompleteness
    | OperationTermination Termination
    | OperationTotallyDefined TotallyDefined
-}

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