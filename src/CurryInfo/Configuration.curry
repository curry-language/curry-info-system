module CurryInfo.Configuration where

import CurryInfo.Types
import CurryInfo.Generator
import CurryInfo.Printer

findDescription :: String -> Configuration a b -> Maybe Description
findDescription field conf = description <$> lookup field conf

findGenerator :: String -> Configuration a b -> Maybe (Generator a b)
findGenerator field conf = generator <$> lookup field conf

findPrinter :: String -> Configuration a b -> Maybe (Printer b)
findPrinter field conf = printer <$> lookup field conf

-- PACKAGE

packageFields :: [String]
packageFields = map fst packageConfiguration

packageConfiguration :: Configuration CurryPackage PackageInformation
packageConfiguration =
    [ ("package",   RegisteredField "\t\t\tThe name of the package" gPackageName pPackageName)
    , ("versions",  RegisteredField "\t\t\tThe versions available of the package" gPackageVersions pPackageVersions)
    ]

-- VERSION

versionFields :: [String]
versionFields = map fst versionConfiguration

versionConfiguration :: Configuration CurryVersion VersionInformation
versionConfiguration =
    [ ("version",       RegisteredField "\t\tThe version number of the version" gVersionVersion pVersionVersion)
    , ("documentation", RegisteredField "\t\tThe documentation of the version" gVersionDocumentation pVersionDocumentation)
    , ("categories",    RegisteredField "\t\tThe categories of the version" gVersionCategories pVersionCategories)
    , ("modules",       RegisteredField "\t\tThe exported modules of the version" gVersionModules pVersionModules)
    , ("dependencies",  RegisteredField "\t\tThe dependencies of the version" gVersionDependencies pVersionDependencies)
    ]

-- MODULE

moduleFields :: [String]
moduleFields = map fst moduleConfiguration

moduleConfiguration :: Configuration CurryModule ModuleInformation
moduleConfiguration =
    [ ("module",        RegisteredField "\t\t\tThe name of the module" gModuleName pModuleName)
    , ("documentation", RegisteredField "\t\tReference to the documentation comment of the module" gModuleDocumentation pModuleDocumentation)
    , ("sourceCode",    RegisteredField "\t\tReference to the source code of the module" gModuleSourceCode pModuleSourceCode)
    , ("safe",          RegisteredField "\t\t\tAnalysis result whether the module is safe" gModuleSafe pModuleSafe)
    --, ("exports",       RegisteredField "" extractModuleExports gModuleExports)
    , ("typeclasses",   RegisteredField "\t\tThe exported typeclasses of the module" gModuleTypeclasses pModuleTypeclasses)
    , ("types",         RegisteredField "\t\t\tThe exported types of the module" gModuleTypes pModuleTypes)
    , ("operations",    RegisteredField "\t\tThe exported operations of the module" gModuleOperations pModuleOperations)
    ]

-- TYPE

typeFields :: [String]
typeFields = map fst typeConfiguration

typeConfiguration :: Configuration CurryType TypeInformation
typeConfiguration =
    [ ("typeName",      RegisteredField "\t\tThe name of the type" gTypeName pTypeName)
    , ("documentation", RegisteredField "\t\tReference to the documentation comment of the type" gTypeDocumentation pTypeDocumentation)
    , ("constructors",  RegisteredField "\t\tThe list of the constructors of the type" gTypeConstructors pTypeConstructors)
    , ("definition",    RegisteredField "\t\tReference to the definition of the type" gTypeDefinition pTypeDefinition)
    ]

-- TYPECLASS

typeclassFields :: [String]
typeclassFields = map fst typeclassConfiguration

typeclassConfiguration :: Configuration CurryTypeclass TypeclassInformation
typeclassConfiguration =
    [ ("typeclass",     RegisteredField "\t\tThe name of the typeclass" gTypeclassName pTypeclassName)
    , ("documentation", RegisteredField "\t\tReference to the documentation comment of the typeclass" gTypeclassDocumentation pTypeclassDocumentation)
    , ("methods",       RegisteredField "\t\tThe list of the methods of the typeclass" gTypeclassMethods pTypeclassMethods)
    , ("definition",    RegisteredField "\t\tReference to the definition of the typeclass" gTypeclassDefinition pTypeclassDefinition)
    ]
    
-- OPERATION

operationFields :: [String]
operationFields = map fst operationConfiguration

operationConfiguration :: Configuration CurryOperation OperationInformation
operationConfiguration =
    [ ("operation",             RegisteredField "\t\tThe name of the operation" gOperationName pOperationName)
    , ("documentation",         RegisteredField "\t\tReference to the documentation comment of the operation" gOperationDocumentation pOperationDocumentation)
    , ("definition",            RegisteredField "\t\tReference to the definition of the operation" gOperationSourceCode pOperationSourceCode)
    , ("signature",             RegisteredField "\t\tThe signature of the operation" gOperationSignature pOperationSignature)
    , ("infix",                 RegisteredField "\t\t\tWhether the operation is infix and in what way (Infix, InfixL, InfixR)" gOperationInfix pOperationInfix)
    , ("precedence",            RegisteredField "\t\tPrecedence of the operation when used infix" gOperationPrecedence pOperationPrecedence)
    , ("deterministic",         RegisteredField "\t\tAnalysis result whether the operation is deterministic" gOperationDeterministic pOperationDeterministic)
    , ("demandness",            RegisteredField "\t\tAnalysis result what arguments are demanded" gOperationDemandness pOperationDemandness)
    , ("indeterministic",       RegisteredField "\tAnalysis result whether the operation is indeterministic" gOperationIndeterministic pOperationIndeterministic)
    , ("solutionCompleteness",  RegisteredField "\tAnalysis result whether the operation is solution complete" gOperationSolutionCompleteness pOperationSolutionCompleteness)
    , ("termination",           RegisteredField "\t\tAnalysis result whether the operation is guaranteed to always terminate" gOperationTermination pOperationTermination)
    , ("totallyDefined",        RegisteredField "\t\tAnalysis result whether the operation is totally defined" gOperationTotallyDefined pOperationTotallyDefined)
    ]