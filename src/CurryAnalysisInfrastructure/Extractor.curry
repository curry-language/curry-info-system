module CurryAnalysisInfrastructure.Extractor where

import CurryAnalysisInfrastructure.Types

import Data.Maybe (listToMaybe)

-- PACKAGE

type PackageExtractor = [PackageInformation] -> Maybe PackageInformation

extractPackageName :: PackageExtractor
extractPackageName infos = listToMaybe $ filter isPackageName infos

extractPackageVersions :: PackageExtractor
extractPackageVersions infos = listToMaybe $ filter isPackageVersions infos

-- VERSION

type VersionExtractor = [VersionInformation] -> Maybe VersionInformation

extractVersionVersion :: VersionExtractor
extractVersionVersion infos = listToMaybe $ filter isVersionVersion infos

extractVersionDocumentation :: VersionExtractor
extractVersionDocumentation infos = listToMaybe $ filter isVersionDocumentation infos

extractVersionCategories :: VersionExtractor
extractVersionCategories infos = listToMaybe $ filter isVersionCategories infos

extractVersionModules :: VersionExtractor
extractVersionModules infos = listToMaybe $ filter isVersionModules infos

-- MODULE

type ModuleExtractor = [ModuleInformation] -> Maybe ModuleInformation

extractModuleName :: ModuleExtractor
extractModuleName infos = listToMaybe $ filter isModuleName infos

extractModuleDocumentation :: ModuleExtractor
extractModuleDocumentation infos = listToMaybe $ filter isModuleDocumentation infos

extractModuleSourceCode :: ModuleExtractor
extractModuleSourceCode infos = listToMaybe $ filter isModuleSourceCode infos

extractModuleUnsafe :: ModuleExtractor
extractModuleUnsafe infos = listToMaybe $ filter isModuleUnsafe infos

extractModuleExports :: ModuleExtractor
extractModuleExports infos = listToMaybe $ filter isModuleExports infos

extractModuleTypeclasses :: ModuleExtractor
extractModuleTypeclasses infos = listToMaybe $ filter isModuleTypeclasses infos

extractModuleTypes :: ModuleExtractor
extractModuleTypes infos = listToMaybe $ filter isModuleTypes infos

extractModuleOperations :: ModuleExtractor
extractModuleOperations infos = listToMaybe $ filter isModuleOperations infos