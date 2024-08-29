module CurryInfo.Configuration where

import CurryInfo.Types
import CurryInfo.Extractor
import CurryInfo.Generator
import CurryInfo.Printer

findDescription :: String -> Configuration a b -> Maybe Description
findDescription field conf = description <$> lookup field conf

findExtractor :: String -> Configuration a b -> Maybe (Extractor b)
findExtractor field conf = extractor <$> lookup field conf

findGenerator :: String -> Configuration a b -> Maybe (Generator a b)
findGenerator field conf = generator <$> lookup field conf

findPrinter :: String -> Configuration a b -> Maybe (Printer b)
findPrinter field conf = printer <$> lookup field conf

-- PACKAGE generate

packageFields :: [String]
packageFields = map fst packageConfiguration

packageConfiguration :: Configuration CurryPackage PackageInformation
packageConfiguration =
    [ ("package",   RegisteredField "\t\t\tThe name of the package"                 extractPackageName      generatePackageName printPackageName)
    , ("versions",  RegisteredField "\t\t\tThe versions available of the package"   extractPackageVersions  generatePackageVersions printPackageVersions)
    ]

-- VERSION

versionFields :: [String]
versionFields = map fst versionConfiguration

versionConfiguration :: Configuration CurryVersion VersionInformation
versionConfiguration =
    [ ("version",       RegisteredField "\t\tThe version number of the version"     extractVersionVersion       generateVersionVersion printVersionVersion)
    , ("documentation", RegisteredField "\t\tThe documentation of the version"      extractVersionDocumentation generateVersionDocumentation printVersionDocumentation)
    , ("categories",    RegisteredField "\t\tThe categories of the version"         extractVersionCategories    generateVersionCategories printVersionCategories)
    , ("modules",       RegisteredField "\t\tThe exported modules of the version"   extractVersionModules       generateVersionModules printVersionModules)
    , ("dependencies",  RegisteredField "\t\tThe dependencies of the version"       extractVersionDependencies  generateVersionDependencies printVersionDependencies)
    ]

-- MODULE

moduleFields :: [String]
moduleFields = map fst moduleConfiguration

moduleConfiguration :: Configuration CurryModule ModuleInformation
moduleConfiguration =
    [ ("module",        RegisteredField "\t\t\tThe name of the module"                              extractModuleName           generateModuleName printModuleName)
    , ("documentation", RegisteredField "\t\tReference to the documentation comment of the module"  extractModuleDocumentation  generateModuleDocumentation printModuleDocumentation)
    , ("sourceCode",    RegisteredField "\t\tReference to the source code of the module"            extractModuleSourceCode     generateModuleSourceCode printModuleSourceCode)
    , ("safe",          RegisteredField "\t\t\tAnalysis result whether the module is safe"          extractModuleSafe           generateModuleSafe printModuleSafe)
    --, ("exports",       RegisteredField "" extractModuleExports generateModuleExports)
    , ("typeclasses",   RegisteredField "\t\tThe exported typeclasses of the module"                extractModuleTypeclasses    generateModuleTypeclasses printModuleTypeclasses)
    , ("types",         RegisteredField "\t\t\tThe exported types of the module"                    extractModuleTypes          generateModuleTypes printModuleTypes)
    , ("operations",    RegisteredField "\t\tThe exported operations of the module"                 extractModuleOperations     generateModuleOperations printModuleOperations)
    ]

-- TYPE

typeFields :: [String]
typeFields = map fst typeConfiguration

typeConfiguration :: Configuration CurryType TypeInformation
typeConfiguration =
    [ ("typeName",      RegisteredField "\t\tThe name of the type"                                  extractTypeName             generateTypeName printTypeName)
    , ("documentation", RegisteredField "\t\tReference to the documentation comment of the type"    extractTypeDocumentation    generateTypeDocumentation printTypeDocumentation)
    , ("constructors",  RegisteredField "\t\tThe list of the constructors of the type"              extractTypeConstructors     generateTypeConstructors printTypeConstructors)
    , ("definition",    RegisteredField "\t\tReference to the definition of the type"               extractTypeDefinition       generateTypeDefinition printTypeDefinition)
    ]

-- TYPECLASS

typeclassFields :: [String]
typeclassFields = map fst typeclassConfiguration

typeclassConfiguration :: Configuration CurryTypeclass TypeclassInformation
typeclassConfiguration =
    [ ("typeclass",     RegisteredField "\t\tThe name of the typeclass"                                 extractTypeclassName            generateTypeclassName printTypeclassName)
    , ("documentation", RegisteredField "\t\tReference to the documentation comment of the typeclass"   extractTypeclassDocumentation   generateTypeclassDocumentation printTypeclassDocumentation)
    , ("methods",       RegisteredField "\t\tThe list of the methods of the typeclass"                  extractTypeclassMethods         generateTypeclassMethods printTypeclassMethods)
    , ("definition",    RegisteredField "\t\tReference to the definition of the typeclass"              extractTypeclassDefinition      generateTypeclassDefinition printTypeclassDefinition)
    ]
    
-- OPERATION

operationFields :: [String]
operationFields = map fst operationConfiguration

operationConfiguration :: Configuration CurryOperation OperationInformation
operationConfiguration =
    [ ("operation",             RegisteredField "\t\tThe name of the operation"                                                 extractOperationName                    generateOperationName printOperationName)
    , ("documentation",         RegisteredField "\t\tReference to the documentation comment of the operation"                   extractOperationDocumentation           generateOperationDocumentation printOperationDocumentation)
    , ("definition",            RegisteredField "\t\tReference to the definition of the operation"                              extractOperationSourceCode              generateOperationSourceCode printOperationSourceCode)
    , ("signature",             RegisteredField "\t\tThe signature of the operation"                                            extractOperationSignature               generateOperationSignature printOperationSignature)
    , ("infix",                 RegisteredField "\t\t\tWhether the operation is infix and in what way (Infix, InfixL, InfixR)"  extractOperationInfix                   generateOperationInfix printOperationInfix)
    , ("precedence",            RegisteredField "\t\tPrecedence of the operation when used infix"                               extractOperationPrecedence              generateOperationPrecedence printOperationPrecedence)
    , ("deterministic",         RegisteredField "\t\tAnalysis result whether the operation is deterministic"                    extractOperationDeterministic           generateOperationDeterministic printOperationDeterministic)
    , ("demandness",            RegisteredField "\t\tAnalysis result what arguments are demanded"                               extractOperationDemandness              generateOperationDemandness printOperationDemandness)
    , ("indeterministic",       RegisteredField "\tAnalysis result whether the operation is indeterministic"                    extractOperationIndeterministic         generateOperationIndeterministic printOperationIndeterministic)
    , ("solutionCompleteness",  RegisteredField "\tAnalysis result whether the operation is solution complete"                  extractOperationSolutionCompleteness    generateOperationSolutionCompleteness printOperationSolutionCompleteness)
    , ("termination",           RegisteredField "\t\tAnalysis result whether the operation is guaranteed to always terminate"   extractOperationTermination             generateOperationTermination printOperationTermination)
    , ("totallyDefined",        RegisteredField "\t\tAnalysis result whether the operation is totally defined"                  extractOperationTotallyDefined          generateOperationTotallyDefined printOperationTotallyDefined)
    ]