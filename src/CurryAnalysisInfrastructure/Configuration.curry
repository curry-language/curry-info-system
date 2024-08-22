module CurryAnalysisInfrastructure.Configuration where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Extractor
import CurryAnalysisInfrastructure.Generator

-- PACKAGE

packageFields :: [String]
packageFields = map fst packageConfiguration

packageConfiguration :: Configuration CurryPackage PackageInformation
packageConfiguration =
    [ ("package",   ("\t\t\tThe name of the package", extractPackageName, generatePackageName))
    , ("versions",  ("\t\t\tThe versions available of the package", extractPackageVersions, generatePackageVersions))
    ]

-- VERSION

versionFields :: [String]
versionFields = map fst versionConfiguration

versionConfiguration :: Configuration CurryVersion VersionInformation
versionConfiguration =
    [ ("version",       ("\t\t\tThe version number of the version", extractVersionVersion, generateVersionVersion))
    , ("documentation", ("\t\t\tThe documentation of the version", extractVersionDocumentation, generateVersionDocumentation))
    , ("categories",    ("\t\t\tThe categories of the version", extractVersionCategories, generateVersionCategories))
    , ("modules",       ("\t\t\tThe exported modules of the version", extractVersionModules, generateVersionModules))
    , ("dependencies",  ("\t\t\tThe dependencies of the version", extractVersionDependencies, generateVersionDependencies))
    ]

-- MODULE

moduleFields :: [String]
moduleFields = map fst moduleConfiguration

moduleConfiguration :: Configuration CurryModule ModuleInformation
moduleConfiguration =
    [ ("module",        ("\t\t\t\tThe name of the module", extractModuleName, generateModuleName))
    , ("documentation", ("\t\t\tReference to the documentation comment of the module", extractModuleDocumentation, generateModuleDocumentation))
    , ("sourceCode",    ("\t\t\tReference to the source code of the module", extractModuleSourceCode, generateModuleSourceCode))
    , ("safe",          ("\t\t\t\tAnalysis result whether the module is safe", extractModuleSafe, generateModuleSafe))
    , ("exports",       ("", extractModuleExports, generateModuleExports))
    , ("typeclasses",   ("\t\t\tThe exported typeclasses of the module", extractModuleTypeclasses, generateModuleTypeclasses))
    , ("types",         ("\t\t\t\tThe exported types of the module", extractModuleTypes, generateModuleTypes))
    , ("operations",    ("\t\t\tThe exported operations of the module", extractModuleOperations, generateModuleOperations))
    ]

-- TYPE

typeFields :: [String]
typeFields = map fst typeConfiguration

typeConfiguration :: Configuration CurryType TypeInformation
typeConfiguration =
    [ ("typeName",      ("\t\t\tThe name of the type", extractTypeName, generateTypeName))
    , ("documentation", ("\t\t\tReference to the documentation comment of the type", extractTypeDocumentation, generateTypeDocumentation))
    , ("constructors",  ("\t\t\tThe list of the constructors of the type", extractTypeConstructors, generateTypeConstructors))
    , ("definition",    ("\t\t\tReference to the definition of the type", extractTypeDefinition, generateTypeDefinition))
    ]

-- TYPECLASS

typeclassFields :: [String]
typeclassFields = map fst typeclassConfiguration

typeclassConfiguration :: Configuration CurryTypeclass TypeclassInformation
typeclassConfiguration =
    [ ("typeclass",     ("\t\t\tThe name of the typeclass", extractTypeclassName, generateTypeclassName))
    , ("documentation", ("\t\t\tReference to the documentation comment of the typeclass", extractTypeclassDocumentation, generateTypeclassDocumentation))
    , ("methods",       ("\t\t\tThe list of the methods of the typeclass", extractTypeclassMethods, generateTypeclassMethods))
    , ("definition",    ("\t\t\tReference to the definition of the typeclass", extractTypeclassDefinition, generateTypeclassDefinition))
    ]
    
-- OPERATION

operationFields :: [String]
operationFields = map fst operationConfiguration

operationConfiguration :: Configuration CurryOperation OperationInformation
operationConfiguration =
    [ ("operation",             ("\t\t\tThe name of the operation", extractOperationName, generateOperationName))
    , ("documentation",         ("\t\t\tReference to the documentation comment of the operation", extractOperationDocumentation, generateOperationDocumentation))
    , ("definition",            ("\t\t\tReference to the definition of the operation", extractOperationSourceCode, generateOperationSourceCode))
    , ("signature",             ("\t\t\tThe signature of the operation", extractOperationSignature, generateOperationSignature))
    , ("infix",                 ("\t\t\t\tWhether the operation is infix and in what way (Infix, InfixL, InfixR)", extractOperationInfix, generateOperationInfix))
    , ("precedence",            ("\t\t\tPrecedence of the operation when used infix", extractOperationPrecedence, generateOperationPrecedence))
    , ("deterministic",         ("\t\t\tAnalysis result whether the operation is deterministic", extractOperationDeterministic, generateOperationDeterministic))
    , ("demandness",            ("\t\t\tAnalysis result what arguments are demanded", extractOperationDemandness, generateOperationDemandness))
    , ("indeterministic",       ("\t\tAnalysis result whether the operation is indeterministic", extractOperationIndeterministic, generateOperationIndeterministic))
    , ("solutionCompleteness",  ("\t\tAnalysis result whether the operation is solution complete", extractOperationSolutionCompleteness, generateOperationSolutionCompleteness))
    , ("termination",           ("\t\t\tAnalysis result whether the operation is guaranteed to always terminate", extractOperationTermination, generateOperationTermination))
    , ("totallyDefined",        ("\t\t\tAnalysis result whether the operation is totally defined", extractOperationTotallyDefined, generateOperationTotallyDefined))
    ]