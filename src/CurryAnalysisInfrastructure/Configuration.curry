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
    [ ("package",   (extractPackageName, generatePackageName))
    , ("versions",  (extractPackageVersions, generatePackageVersions))
    ]

-- VERSION

versionFields :: [String]
versionFields = map fst versionConfiguration

versionConfiguration :: Configuration CurryVersion VersionInformation
versionConfiguration =
    [ ("version",       (extractVersionVersion, generateVersionVersion))
    , ("documentation", (extractVersionDocumentation, generateVersionDocumentation))
    , ("categories",    (extractVersionCategories, generateVersionCategories))
    , ("modules",       (extractVersionModules, generateVersionModules))
    , ("dependencies",  (extractVersionDependencies, generateVersionDependencies))
    ]

-- MODULE

moduleFields :: [String]
moduleFields = map fst moduleConfiguration

moduleConfiguration :: Configuration CurryModule ModuleInformation
moduleConfiguration =
    [ ("module",        (extractModuleName, generateModuleName))
    , ("documentation", (extractModuleDocumentation, generateModuleDocumentation))
    , ("sourceCode",    (extractModuleSourceCode, generateModuleSourceCode))
    , ("safe",          (extractModuleSafe, generateModuleSafe))
    , ("exports",       (extractModuleExports, generateModuleExports))
    , ("typeclasses",   (extractModuleTypeclasses, generateModuleTypeclasses))
    , ("types",         (extractModuleTypes, generateModuleTypes))
    , ("operations",    (extractModuleOperations, generateModuleOperations))
    ]

-- TYPE

typeFields :: [String]
typeFields = map fst typeConfiguration

typeConfiguration :: Configuration CurryType TypeInformation
typeConfiguration =
    [ ("typeName",      (extractTypeName, generateTypeName))
    , ("documentation", (extractTypeDocumentation, generateTypeDocumentation))
    , ("constructors",  (extractTypeConstructors, generateTypeConstructors))
    , ("definition",    (extractTypeDefinition, generateTypeDefinition))
    ]

-- TYPECLASS

typeclassFields :: [String]
typeclassFields = map fst typeclassConfiguration

typeclassConfiguration :: Configuration CurryTypeclass TypeclassInformation
typeclassConfiguration =
    [ ("typeclass", (extractTypeclassName, generateTypeclassName))
    , ("documentation", (extractTypeclassDocumentation, generateTypeclassDocumentation))
    , ("methods", (extractTypeclassMethods, generateTypeclassMethods))
    , ("definition", (extractTypeclassDefinition, generateTypeclassDefinition))
    ]
    
-- OPERATION

operationFields :: [String]
operationFields = map fst operationConfiguration

operationConfiguration :: Configuration CurryOperation OperationInformation
operationConfiguration =
    [ ("operation",             (extractOperationName, generateOperationName))
    , ("documentation",         (extractOperationDocumentation, generateOperationDocumentation))
    , ("sourceCode",            (extractOperationSourceCode, generateOperationSourceCode))
    , ("signature",             (extractOperationSignature, generateOperationSignature))
    , ("infix",                 (extractOperationInfix, generateOperationInfix))
    , ("precedence",            (extractOperationPrecedence, generateOperationPrecedence))
    , ("deterministic",         (extractOperationDeterministic, generateOperationDeterministic))
    , ("demandness",            (extractOperationDemandness, generateOperationDemandness))
    , ("indeterministic",       (extractOperationIndeterministic, generateOperationIndeterministic))
    , ("solutionCompleteness",  (extractOperationSolutionCompleteness, generateOperationSolutionCompleteness))
    , ("termination",           (extractOperationTermination, generateOperationTermination))
    , ("totallyDefined",        (extractOperationTotallyDefined, generateOperationTotallyDefined))
    ]