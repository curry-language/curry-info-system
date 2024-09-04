module CurryInfo.Types where

import Text.Pretty (Doc)

import JSON.Data (JValue)

-- Generator Type

type Generator a b = Options -> a -> IO (Maybe (String, b))

-- Printer Type

type Printer a = Options -> a -> IO String

-- Configuration Type

type Description = String
--type Configuration a b = [(String, (Description, Extractor b, Generator a b))]
type Configuration a b = [(String, RegisteredField a b)]
data RegisteredField a b = RegisteredField
    { description   :: Description
    , generator     :: Generator a b
    , printer       :: Printer b
    }

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
    , optOutput     :: String       -- The output format
    , optClean      :: Bool         -- Clean up information
    }
    deriving Show 

class Field a where
    fieldName :: a -> String

-- OUTPUT TYPE

data Output
    = OutputText String
    | OutputError String
    | OutputJSON JValue
    deriving Show

-- INPUT TYPES

data CurryPackage = CurryPackage Package

instance Field PackageInformation where
    fieldName info = case info of
        PackageName _ -> "package"
        PackageVersions _ -> "versions"

data CurryVersion = CurryVersion Package Version

instance Field VersionInformation where
    fieldName info = case info of
        VersionVersion _ -> "version"
        VersionDocumentation _ -> "documentation"
        VersionCategories _ -> "categories"
        VersionModules _ -> "modules"
        VersionDependencies _ -> "dependencies"

data CurryModule = CurryModule Package Version Module

instance Field ModuleInformation where
    fieldName info = case info of
        ModuleName _ -> "module"
        ModuleDocumentation _ -> "documentation"
        ModuleSourceCode _ -> "sourceCode"
        ModuleSafe _ -> "safe"
        ModuleTypeclasses _ -> "typeclasses"
        ModuleTypes _ -> "types"
        ModuleOperations _ -> "operations"

data CurryType = CurryType Package Version Module Type

instance Field TypeInformation where
    fieldName info = case info of
        TypeName _ -> "typeName"
        TypeDocumentation _ -> "documentation"
        TypeConstructors _ -> "constructors"
        TypeDefinition _ -> "definition"

data CurryTypeclass = CurryTypeclass Package Version Module Typeclass

instance Field TypeclassInformation where
    fieldName info = case info of
        TypeclassName _ -> "typeclass"
        TypeclassDocumentation _ -> "documentation"
        TypeclassMethods _ -> "methods"
        TypeclassDefinition _ -> "definition"

data CurryOperation = CurryOperation Package Version Module Operation

instance Field OperationInformation where
    fieldName info = case info of
        OperationName _ -> "operation"
        OperationDocumentation _ -> "documentation"
        OperationSourceCode _ -> "sourceCode"
        OperationSignature _ -> "signature"
        OperationInfix _ -> "infix"
        OperationPrecedence _ -> "precedence"
        OperationDeterministic _ -> "deterministic"
        OperationDemandness _ -> "demandness"
        OperationIndeterministic _ -> "indeterministic"
        OperationSolutionCompleteness _ -> "solutionCompleteness"
        OperationTermination _ -> "termination"
        OperationTotallyDefined _ -> "totallyDefined"

-- INFORMATION TYPES

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

type Demandness = [Float]

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