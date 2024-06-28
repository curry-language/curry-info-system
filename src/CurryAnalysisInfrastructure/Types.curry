module CurryAnalysisInfrastructure.Types where

import Text.Pretty (Doc)

class EqInfo a where
    sameInfo :: a -> a -> Bool

instance EqInfo PackageInformation where
    sameInfo x y = case (x, y) of
        (PackageName _, PackageName _) -> True
        (PackageVersions _, PackageVersions _) -> True
        _ -> False

instance EqInfo VersionInformation where
    sameInfo x y = case (x, y) of
        (VersionVersion _, VersionVersion _) -> True
        (VersionDocumentation _, VersionDocumentation _) -> True
        (VersionCategories _, VersionCategories _) -> True
        (VersionModules _, VersionModules _) -> True
        _ -> False

instance EqInfo ModuleInformation where
    sameInfo x y = case (x, y) of
        (ModuleName _, ModuleName _) -> True
        (ModuleDocumentation _, ModuleDocumentation _) -> True
        (ModuleSourceCode _, ModuleSourceCode _) -> True
        (ModuleUnsafe _, ModuleUnsafe _) -> True
        (ModuleExports _, ModuleExports _) -> True
        (ModuleTypeclasses _, ModuleTypeclasses _) -> True
        (ModuleTypes _, ModuleTypes _) -> True
        (ModuleOperations _, ModuleOperations _) -> True
        _ -> False

-- OUTPUT TYPE

data Output
    = OutputText String
    | OutputError String

-- INPUT TYPES

data CurryPackage = CurryPackage Package

data CurryVersion = CurryVersion Package Version

data CurryModule = CurryModule Package Version Module

data CurryType = CurryType Package Version Module Type

data CurryTypeclass = CurryTypeclass Package Version Module Typeclass

data CurryOperation = CurryOperation Package Version Module Operation

-- INFORMATION TYPES

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
    = ModuleName String
    | ModuleDocumentation Doc
    | ModuleSourceCode Doc
    | ModuleUnsafe Bool
    | ModuleExports [Export]
    | ModuleTypeclasses [Typeclass]
    | ModuleTypes [Type]
    | ModuleOperations [Operation]

isModuleName :: ModuleInformation -> Bool
isModuleName x = case x of
    ModuleName _ -> True 
    _ -> False

isModuleDocumentation :: ModuleInformation -> Bool
isModuleDocumentation x = case x of
    ModuleDocumentation _ -> True 
    _ -> False

isModuleSourceCode :: ModuleInformation -> Bool
isModuleSourceCode x = case x of
    ModuleSourceCode _ -> True 
    _ -> False

isModuleUnsafe :: ModuleInformation -> Bool
isModuleUnsafe x = case x of
    ModuleUnsafe _ -> True 
    _ -> False

isModuleExports :: ModuleInformation -> Bool
isModuleExports x = case x of
    ModuleExports _ -> True 
    _ -> False

isModuleTypeclasses :: ModuleInformation -> Bool
isModuleTypeclasses x = case x of
    ModuleTypeclasses _ -> True 
    _ -> False

isModuleTypes :: ModuleInformation -> Bool
isModuleTypes x = case x of
    ModuleTypes _ -> True 
    _ -> False

isModuleOperations :: ModuleInformation -> Bool
isModuleOperations x = case x of
    ModuleOperations _ -> True 
    _ -> False

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

type Package = String