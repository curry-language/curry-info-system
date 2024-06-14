module CurryAnalysisInfrastructure.Types where

import Text.Pretty (Doc)

data PackageInformation
    = PackageName String
    | PackageVersions [Version]

isPackageName :: PackageInformation -> Bool
isPackageName x = case x of
    PackageName _ -> True
    _ -> False

isPackageVersions :: PackageInformation -> Bool
isPackageVersions x = case x of
    PackageVersions _ -> True
    _ -> False

data VersionInformation
    = VersionVersion Version
    | VersionDocumentation Doc
    | VersionCategories [Category]
    | VersionModules [Module]

isVersionVersion :: VersionInformation -> Bool
isVersionVersion x = case x of
    VersionVersion _ -> True
    _ -> False

isVersionDocumentation :: VersionInformation -> Bool
isVersionDocumentation x = case x of
    VersionDocumentation _ -> True
    _ -> False

isVersionCategories :: VersionInformation -> Bool
isVersionCategories x = case x of
    VersionCategories _ -> True
    _ -> False

isVersionModules :: VersionInformation -> Bool
isVersionModules x = case x of
    VersionModules _ -> True
    _ -> False

data ModuleInformation
    = ModuleDocumentation Doc
    | ModuleSourceCode Doc
    | ModuleUnsafe Bool
    | ModuleExports [Export]
    | ModuleTypeclasses [Typeclass]
    | ModuleTypes [Type]
    | ModuleOperations [Operation]

data TypeInformation
    = TypeDocumentation Doc
    | TypeConstructors (Either External [Constructor])
    | TypeDefinition Doc

data TypeclassInformation
    = TypeclassDocumentation Doc
    | TypeclassMethods [Signature]
    | TypeclassDefinition Doc

data OperationInformation
    = OperationDocumentation Doc
    | OperationSourceCode Doc
    | OperationSignature Signature
    | OperationInfix Infix
    | OperationPrecedence Precedence
    | OperationDeterminism Determinism
    | OperationDemandness [Int]
    | OperationIndeterminism Bool
    | OperationSolutionCompleteness Bool
    | OperationTermination Bool
    | OperationTotallyDefined Bool

data Infix = Infix | InfixL | InfixR
    deriving (Read, Show)

type Precedence = Int

data Determinism = Det | NDet
    deriving (Read, Show)

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