module CurryInfo.Extractor where

import CurryInfo.Types

import Data.List (find)

defaultExtractor :: (a -> Bool) -> [a] -> Maybe a
defaultExtractor p = find p

-- PACKAGE

type PackageExtractor = Extractor PackageInformation

extractPackageName :: PackageExtractor
extractPackageName = defaultExtractor isPackageName

extractPackageVersions :: PackageExtractor
extractPackageVersions = defaultExtractor isPackageVersions

-- VERSION

type VersionExtractor = Extractor VersionInformation

extractVersionVersion :: VersionExtractor
extractVersionVersion = defaultExtractor isVersionVersion

extractVersionDocumentation :: VersionExtractor
extractVersionDocumentation = defaultExtractor isVersionDocumentation

extractVersionCategories :: VersionExtractor
extractVersionCategories = defaultExtractor isVersionCategories

extractVersionModules :: VersionExtractor
extractVersionModules = defaultExtractor isVersionModules

extractVersionDependencies :: VersionExtractor
extractVersionDependencies = defaultExtractor isVersionDependencies

-- MODULE

type ModuleExtractor = Extractor ModuleInformation

extractModuleName :: ModuleExtractor
extractModuleName = defaultExtractor isModuleName

extractModuleDocumentation :: ModuleExtractor
extractModuleDocumentation = defaultExtractor isModuleDocumentation

extractModuleSourceCode :: ModuleExtractor
extractModuleSourceCode = defaultExtractor isModuleSourceCode

extractModuleSafe :: ModuleExtractor
extractModuleSafe = defaultExtractor isModuleSafe

extractModuleExports :: ModuleExtractor
extractModuleExports = defaultExtractor isModuleExports

extractModuleTypeclasses :: ModuleExtractor
extractModuleTypeclasses = defaultExtractor isModuleTypeclasses

extractModuleTypes :: ModuleExtractor
extractModuleTypes = defaultExtractor isModuleTypes

extractModuleOperations :: ModuleExtractor
extractModuleOperations = defaultExtractor isModuleOperations

-- TYPE

type TypeExtractor = Extractor TypeInformation

extractTypeName :: TypeExtractor
extractTypeName = defaultExtractor isTypeName

extractTypeDocumentation :: TypeExtractor
extractTypeDocumentation = defaultExtractor isTypeDocumentation

extractTypeConstructors :: TypeExtractor
extractTypeConstructors = defaultExtractor isTypeConstructors

extractTypeDefinition :: TypeExtractor
extractTypeDefinition = defaultExtractor isTypeDefinition

-- TYPECLASS

type TypeclassExtractor = Extractor TypeclassInformation

extractTypeclassName :: TypeclassExtractor
extractTypeclassName = defaultExtractor isTypeclassName

extractTypeclassDocumentation :: TypeclassExtractor
extractTypeclassDocumentation = defaultExtractor isTypeclassDocumentation

extractTypeclassMethods :: TypeclassExtractor
extractTypeclassMethods = defaultExtractor isTypeclassMethods

extractTypeclassDefinition :: TypeclassExtractor
extractTypeclassDefinition = defaultExtractor isTypeclassDefinition

-- OPERATION

type OperationExtractor = Extractor OperationInformation

extractOperationName :: OperationExtractor
extractOperationName = defaultExtractor isOperationName

extractOperationDocumentation :: OperationExtractor
extractOperationDocumentation = defaultExtractor isOperationDocumentation

extractOperationSourceCode :: OperationExtractor
extractOperationSourceCode = defaultExtractor isOperationSourceCode

extractOperationSignature :: OperationExtractor
extractOperationSignature = defaultExtractor isOperationSignature

extractOperationInfix :: OperationExtractor
extractOperationInfix = defaultExtractor isOperationInfix

extractOperationPrecedence :: OperationExtractor
extractOperationPrecedence = defaultExtractor isOperationPrecedence

extractOperationDeterministic :: OperationExtractor
extractOperationDeterministic = defaultExtractor isOperationDeterministic

extractOperationDemandness :: OperationExtractor
extractOperationDemandness = defaultExtractor isOperationDemandness

extractOperationIndeterministic :: OperationExtractor
extractOperationIndeterministic = defaultExtractor isOperationIndeterministic

extractOperationSolutionCompleteness :: OperationExtractor
extractOperationSolutionCompleteness = defaultExtractor isOperationSolutionCompleteness

extractOperationTermination :: OperationExtractor
extractOperationTermination = defaultExtractor isOperationTermination

extractOperationTotallyDefined :: OperationExtractor
extractOperationTotallyDefined = defaultExtractor isOperationTotallyDefined
