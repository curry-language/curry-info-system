module CurryAnalysisInfrastructure.Types where

import Text.Pretty (Doc)

class EqInfo a where
    sameInfo :: a -> a -> Bool

instance EqInfo PackageInformation where
    sameInfo x y = case (x, y) of
        (PackageName _      , PackageName _)        -> True
        (PackageVersions _  , PackageVersions _)    -> True
        _                                           -> False

instance EqInfo VersionInformation where
    sameInfo x y = case (x, y) of
        (VersionVersion _       , VersionVersion _)         -> True
        (VersionDocumentation _ , VersionDocumentation _)   -> True
        (VersionCategories _    , VersionCategories _)      -> True
        (VersionModules _       , VersionModules _)         -> True
        _                                                   -> False

instance EqInfo ModuleInformation where
    sameInfo x y = case (x, y) of
        (ModuleName _           , ModuleName _)             -> True
        (ModuleDocumentation _  , ModuleDocumentation _)    -> True
        (ModuleSourceCode _     , ModuleSourceCode _)       -> True
        (ModuleSafe _           , ModuleSafe _)             -> True
        (ModuleExports _        , ModuleExports _)          -> True
        (ModuleTypeclasses _    , ModuleTypeclasses _)      -> True
        (ModuleTypes _          , ModuleTypes _)            -> True
        (ModuleOperations _     , ModuleOperations _)       -> True
        _ -> False

instance EqInfo TypeInformation where
    sameInfo x y = case (x, y) of
        (TypeName _         , TypeName _)           -> True
        (TypeDocumentation _, TypeDocumentation _)  -> True
        (TypeConstructors _ , TypeConstructors _)   -> True
        (TypeDefinition _   , TypeDefinition _)     -> True
        _                                           -> False

instance EqInfo TypeclassInformation where
    sameInfo x y = case (x, y) of
        (TypeclassName _            , TypeclassName _)          -> True
        (TypeclassDocumentation _   , TypeclassDocumentation _) -> True
        (TypeclassMethods _         , TypeclassMethods _)       -> True
        (TypeclassDefinition _      , TypeclassDefinition _)    -> True
        _                                                       -> False

instance EqInfo OperationInformation where
    sameInfo x y = case (x, y) of
        (OperationName _                , OperationName _)                  -> True
        (OperationDocumentation _       , OperationDocumentation _)         -> True
        (OperationSourceCode _          , OperationSourceCode _)            -> True
        (OperationSignature _           , OperationSignature _)             -> True
        (OperationInfix _               , OperationInfix _)                 -> True
        (OperationPrecedence _          , OperationPrecedence _)            -> True
        (OperationDeterministic _       , OperationDeterministic _)         -> True
        (OperationDemandness _          , OperationDemandness _)            -> True
        (OperationIndeterministic _     , OperationIndeterministic _)       -> True
        (OperationSolutionCompleteness _, OperationSolutionCompleteness _)  -> True
        (OperationTermination _         , OperationTermination _)           -> True
        (OperationTotallyDefined _      , OperationTotallyDefined _)        -> True
        _                                                                   -> False

-- OUTPUT TYPE

data Output
    = OutputText String
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
    | ModuleSafe Safe
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

isModuleSafe :: ModuleInformation -> Bool
isModuleSafe x = case x of
    ModuleSafe _ -> True 
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
    = TypeName String
    | TypeDocumentation Doc
    | TypeConstructors [Constructor]
    | TypeDefinition Doc

isTypeName :: TypeInformation -> Bool
isTypeName x = case x of
    TypeName _ -> True
    _ -> False

isTypeDocumentation :: TypeInformation -> Bool
isTypeDocumentation x = case x of
    TypeDocumentation _ -> True
    _ -> False

isTypeConstructors :: TypeInformation -> Bool
isTypeConstructors x = case x of
    TypeConstructors _ -> True
    _ -> False

isTypeDefinition :: TypeInformation -> Bool
isTypeDefinition x = case x of
    TypeDefinition _ -> True
    _ -> False

data TypeclassInformation
    = TypeclassName String
    | TypeclassDocumentation Doc
    | TypeclassMethods [Signature]
    | TypeclassDefinition Doc

isTypeclassName :: TypeclassInformation -> Bool
isTypeclassName x = case x of
    TypeclassName _ -> True
    _ -> False

isTypeclassDocumentation :: TypeclassInformation -> Bool
isTypeclassDocumentation x = case x of
    TypeclassDocumentation _ -> True
    _ -> False

isTypeclassMethods :: TypeclassInformation -> Bool
isTypeclassMethods x = case x of
    TypeclassMethods _ -> True
    _ -> False

isTypeclassDefinition :: TypeclassInformation -> Bool
isTypeclassDefinition x = case x of
    TypeclassDefinition _ -> True
    _ -> False

data OperationInformation
    = OperationName String
    | OperationDocumentation Doc
    | OperationSourceCode Doc
    | OperationSignature Signature
    | OperationInfix Infix
    | OperationPrecedence Precedence
    | OperationDeterministic Deterministic
    | OperationDemandness Demandness
    | OperationIndeterministic Indeterministic
    | OperationSolutionCompleteness SolutionCompleteness
    | OperationTermination Termination
    | OperationTotallyDefined TotallyDefined

isOperationName :: OperationInformation -> Bool
isOperationName x = case x of
    OperationName _ -> True
    _ -> False

isOperationDocumentation :: OperationInformation -> Bool
isOperationDocumentation x = case x of
    OperationDocumentation _ -> True
    _ -> False

isOperationSourceCode :: OperationInformation -> Bool
isOperationSourceCode x = case x of
    OperationSourceCode _ -> True
    _ -> False

isOperationSignature :: OperationInformation -> Bool
isOperationSignature x = case x of
    OperationSignature _ -> True
    _ -> False

isOperationInfix :: OperationInformation -> Bool
isOperationInfix x = case x of
    OperationInfix _ -> True
    _ -> False

isOperationPrecedence :: OperationInformation -> Bool
isOperationPrecedence x = case x of
    OperationPrecedence _ -> True
    _ -> False

isOperationDeterministic :: OperationInformation -> Bool
isOperationDeterministic x = case x of
    OperationDeterministic _ -> True
    _ -> False

isOperationDemandness :: OperationInformation -> Bool
isOperationDemandness x = case x of
    OperationDemandness _ -> True
    _ -> False

isOperationIndeterministic :: OperationInformation -> Bool
isOperationIndeterministic x = case x of
    OperationIndeterministic _ -> True
    _ -> False

isOperationSolutionCompleteness :: OperationInformation -> Bool
isOperationSolutionCompleteness x = case x of
    OperationSolutionCompleteness _ -> True
    _ -> False

isOperationTermination :: OperationInformation -> Bool
isOperationTermination x = case x of
    OperationTermination _ -> True
    _ -> False

isOperationTotallyDefined :: OperationInformation -> Bool
isOperationTotallyDefined x = case x of
    OperationTotallyDefined _ -> True
    _ -> False

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