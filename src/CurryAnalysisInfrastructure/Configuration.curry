module CurryAnalysisInfrastructure.Configuration where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Extractor
import CurryAnalysisInfrastructure.Generator

type Configuration a b = [(String, (Extractor b, Generator a b))]

-- PACKAGE

packageFields :: [String]
packageFields = map fst packageConfiguration

packageConfiguration :: Configuration CurryPackage PackageInformation
packageConfiguration =
    [ ("Package", (extractPackageName, generatePackageName))
    , ("Versions", (extractPackageVersions, generatePackageVersions))
    ]

-- VERSION

versionFields :: [String]
versionFields = map fst versionConfiguration

versionConfiguration :: Configuration CurryVersion VersionInformation
versionConfiguration =
    [ ("Version", (extractVersionVersion, generateVersionVersion))
    , ("Documentation", (extractVersionDocumentation, generateVersionDocumentation))
    , ("Categories", (extractVersionCategories, generateVersionCategories))
    , ("Modules", (extractVersionModules, generateVersionModules))
    ]

-- MODULE

moduleFields :: [String]
moduleFields = map fst moduleConfiguration

moduleConfiguration :: Configuration CurryModule ModuleInformation
moduleConfiguration =
    [ ("Module", (extractModuleName, generateModuleName))
    , ("Documentation", (extractModuleDocumentation, generateModuleDocumentation))
    , ("SourceCode", (extractModuleSourceCode, generateModuleSourceCode))
    , ("Safe", (extractModuleSafe, generateModuleSafe))
    , ("Exports", (extractModuleExports, generateModuleExports))
    , ("Typeclasses", (extractModuleTypeclasses, generateModuleTypeclasses))
    , ("Types", (extractModuleTypes, generateModuleTypes))
    , ("Operations", (extractModuleOperations, generateModuleOperations))
    ]
