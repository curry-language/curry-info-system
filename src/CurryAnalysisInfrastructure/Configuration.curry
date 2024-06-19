module CurryAnalysisInfrastructure.Configuration where

import CurryAnalysisInfrastructure.Extractor
import CurryAnalysisInfrastructure.Generator

-- PACKAGE

packageFields :: [String]
packageFields = map fst packageConfiguration

packageConfiguration :: [(String, (PackageExtractor, PackageGenerator))]
packageConfiguration =
    [ ("Package", (extractPackageName, generatePackageName))
    , ("Versions", (extractPackageVersions, generatePackageVersions))
    ]

-- VERSION

versionFields :: [String]
versionFields = map fst versionConfiguration

versionConfiguration :: [(String, (VersionExtractor, VersionGenerator))]
versionConfiguration =
    [ ("Version", (extractVersionVersion, generateVersionVersion))
    , ("Documentation", (extractVersionDocumentation, generateVersionDocumentation))
    , ("Categories", (extractVersionCategories, generateVersionCategories))
    , ("Modules", (extractVersionModules, generateVersionModules))
    ]

-- MODULE

moduleFields :: [String]
moduleFields = map fst moduleConfiguration

moduleConfiguration :: [(String, (ModuleExtractor, ModuleGenerator))]
moduleConfiguration =
    [ ("Module", (extractModuleName, generateModuleName))
    , ("Documentation", (extractModuleDocumentation, generateModuleDocumentation))
    , ("SourceCode", (extractModuleSourceCode, generateModuleSourceCode))
    , ("Unsafe", (extractModuleUnsafe, generateModuleUnsafe))
    , ("Exports", (extractModuleExports, generateModuleExports))
    , ("Typeclasses", (extractModuleTypeclasses, generateModuleTypeclasses))
    , ("Types", (extractModuleTypes, generateModuleTypes))
    , ("Operations", (extractModuleOperations, generateModuleOperations))
    ]
