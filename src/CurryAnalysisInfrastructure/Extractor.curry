module CurryAnalysisInfrastructure.Extractor where

import CurryAnalysisInfrastructure.Types

import Data.Maybe (listToMaybe)

type Extractor a = [a] -> Maybe a

-- PACKAGE

type PackageExtractor = Extractor PackageInformation

extractPackageName :: PackageExtractor
extractPackageName infos = listToMaybe $ filter isPackageName infos

extractPackageVersions :: PackageExtractor
extractPackageVersions infos = listToMaybe $ filter isPackageVersions infos

-- VERSION

type VersionExtractor = Extractor VersionInformation

extractVersionVersion :: VersionExtractor
extractVersionVersion infos = listToMaybe $ filter isVersionVersion infos

extractVersionDocumentation :: VersionExtractor
extractVersionDocumentation infos = listToMaybe $ filter isVersionDocumentation infos

extractVersionCategories :: VersionExtractor
extractVersionCategories infos = listToMaybe $ filter isVersionCategories infos

extractVersionModules :: VersionExtractor
extractVersionModules infos = listToMaybe $ filter isVersionModules infos

extractVersionDependencies :: VersionExtractor
extractVersionDependencies infos = listToMaybe $ filter isVersionDependencies infos

-- MODULE

type ModuleExtractor = Extractor ModuleInformation

extractModuleName :: ModuleExtractor
extractModuleName infos = listToMaybe $ filter isModuleName infos

extractModuleDocumentation :: ModuleExtractor
extractModuleDocumentation infos = listToMaybe $ filter isModuleDocumentation infos

extractModuleSourceCode :: ModuleExtractor
extractModuleSourceCode infos = listToMaybe $ filter isModuleSourceCode infos

extractModuleSafe :: ModuleExtractor
extractModuleSafe infos = listToMaybe $ filter isModuleSafe infos

extractModuleExports :: ModuleExtractor
extractModuleExports infos = listToMaybe $ filter isModuleExports infos

extractModuleTypeclasses :: ModuleExtractor
extractModuleTypeclasses infos = listToMaybe $ filter isModuleTypeclasses infos

extractModuleTypes :: ModuleExtractor
extractModuleTypes infos = listToMaybe $ filter isModuleTypes infos

extractModuleOperations :: ModuleExtractor
extractModuleOperations infos = listToMaybe $ filter isModuleOperations infos

-- TYPE

type TypeExtractor = Extractor TypeInformation

extractTypeName :: TypeExtractor
extractTypeName = listToMaybe . (filter isTypeName)

extractTypeDocumentation :: TypeExtractor
extractTypeDocumentation = listToMaybe . (filter isTypeDocumentation)

extractTypeConstructors :: TypeExtractor
extractTypeConstructors = listToMaybe . (filter isTypeConstructors)

extractTypeDefinition :: TypeExtractor
extractTypeDefinition = listToMaybe . (filter isTypeDefinition)

-- TYPECLASS

type TypeclassExtractor = Extractor TypeclassInformation

extractTypeclassName :: TypeclassExtractor
extractTypeclassName = listToMaybe . (filter isTypeclassName)

extractTypeclassDocumentation :: TypeclassExtractor
extractTypeclassDocumentation = listToMaybe . (filter isTypeclassDocumentation)

extractTypeclassMethods :: TypeclassExtractor
extractTypeclassMethods = listToMaybe . (filter isTypeclassMethods)

extractTypeclassDefinition :: TypeclassExtractor
extractTypeclassDefinition = listToMaybe . (filter isTypeclassDefinition)

-- OPERATION

type OperationExtractor = Extractor OperationInformation

extractOperationName :: OperationExtractor
extractOperationName = listToMaybe . (filter isOperationName)

extractOperationDocumentation :: OperationExtractor
extractOperationDocumentation = listToMaybe . (filter isOperationDocumentation)

extractOperationSourceCode :: OperationExtractor
extractOperationSourceCode = listToMaybe . (filter isOperationSourceCode)

extractOperationSignature :: OperationExtractor
extractOperationSignature = listToMaybe . (filter isOperationSignature)

extractOperationInfix :: OperationExtractor
extractOperationInfix = listToMaybe . (filter isOperationInfix)

extractOperationPrecedence :: OperationExtractor
extractOperationPrecedence = listToMaybe . (filter isOperationPrecedence)

extractOperationDeterministic :: OperationExtractor
extractOperationDeterministic = listToMaybe . (filter isOperationDeterministic)

extractOperationDemandness :: OperationExtractor
extractOperationDemandness = listToMaybe . (filter isOperationDemandness)

extractOperationIndeterministic :: OperationExtractor
extractOperationIndeterministic = listToMaybe . (filter isOperationIndeterministic)

extractOperationSolutionCompleteness :: OperationExtractor
extractOperationSolutionCompleteness = listToMaybe . (filter isOperationSolutionCompleteness)

extractOperationTermination :: OperationExtractor
extractOperationTermination = listToMaybe . (filter isOperationTermination)

extractOperationTotallyDefined :: OperationExtractor
extractOperationTotallyDefined = listToMaybe . (filter isOperationTotallyDefined)
