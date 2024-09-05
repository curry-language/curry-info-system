module CurryInfo.JPretty where

import CurryInfo.Types

import JSON.Data
import JSON.Pretty (ppJSON)
import Text.Pretty

-- This operation takes a list of fields and generates a json object with the given fields.
json :: JPretty a => [a] -> JValue
json infos = JObject (map jpretty infos)

-- This operation generates only the json value for a field, excluding the field name.
jsonValue :: JPretty a => a -> JValue
jsonValue = snd . jpretty

-- This operation generates the fieldname and fieldvalue for the given argument.
jsonOutput :: JPretty a => a -> (String, String)
jsonOutput x =
    let (f, v) = jpretty x in (f, ppJSON v)

class JPretty a where
    jpretty :: a -> JField

-- PACKAGE

jsPackageName :: PackageInformation -> JValue
jsPackageName (PackageName s) = JString s

jsPackageVersions :: PackageInformation -> JValue
jsPackageVersions (PackageVersions vsns) = JArray (map JString vsns)

instance JPretty PackageInformation where
    jpretty (PackageName s)         = ("package", JString s)
    jpretty (PackageVersions vsns)  = ("versions", JArray (map JString vsns))

-- VERSION

jsVersionVersion :: VersionInformation -> JValue
jsVersionVersion (VersionVersion vsn) = JString vsn

jsVersionDocumentation :: VersionInformation -> JValue
jsVersionDocumentation (VersionDocumentation d) = JString d

jsVersionCategories :: VersionInformation -> JValue
jsVersionCategories (VersionCategories cats) = JArray (map JString cats)

jsVersionModules :: VersionInformation -> JValue
jsVersionModules (VersionModules mods) = JArray (map JString mods)

jsVersionDependencies :: VersionInformation -> JValue
jsVersionDependencies (VersionDependencies deps) = JArray (map (JString . show) deps)

instance JPretty VersionInformation where
    jpretty (VersionVersion vsn)            = ("version", JString vsn)
    jpretty (VersionDocumentation d)        = ("documentation", JString d)
    jpretty (VersionCategories cats)        = ("categories", JArray (map JString cats))
    jpretty (VersionModules mods)           = ("modules", JArray (map JString mods))
    jpretty (VersionDependencies deps)      = ("dependencies", JArray (map (JString . show) deps))
        --where
        --f (pkg, lb, ub) = JObject [("package", JString pkg), ("lowerBound", JString (show lb)), ("upperBound", JString (show ub))]

-- MODULE

jsModuleName :: ModuleInformation -> JValue
jsModuleName (ModuleName m) = JString m

jsModuleDocumentation :: ModuleInformation -> JValue
jsModuleDocumentation (ModuleDocumentation d) = JString $ show d

jsModuleSourceCode :: ModuleInformation -> JValue
jsModuleSourceCode (ModuleSourceCode d) = JString $ show d

jsModuleSafe :: ModuleInformation -> JValue
jsModuleSafe (ModuleSafe s) = JString $ show s

jsModuleTypeclasses :: ModuleInformation -> JValue
jsModuleTypeclasses (ModuleTypeclasses tcs) = JArray (map JString tcs)

jsModuleTypes :: ModuleInformation -> JValue
jsModuleTypes (ModuleTypes ts) = JArray (map JString ts)

jsModuleOperations :: ModuleInformation -> JValue
jsModuleOperations (ModuleOperations ops) = JArray (map JString ops)

instance JPretty ModuleInformation where
    jpretty (ModuleName m)          = ("module", JString m)
    jpretty (ModuleDocumentation d) = ("documentation", JString $ show d)
    jpretty (ModuleSourceCode d)    = ("sourceCode", JString $ show d)
    jpretty (ModuleSafe s)          = ("safe", JString $ show s)
    jpretty (ModuleExports es)      = ("exports", JArray (map JString es))
    jpretty (ModuleTypeclasses tcs) = ("typeclasses", JArray (map JString tcs))
    jpretty (ModuleTypes ts)        = ("types", JArray (map JString ts))
    jpretty (ModuleOperations ops)  = ("operations", JArray (map JString ops))

-- TYPE

jsTypeName :: TypeInformation -> JValue
jsTypeName (TypeName n) = JString n

jsTypeDocumentation :: TypeInformation -> JValue
jsTypeDocumentation (TypeDocumentation d) = JString $ show d

jsTypeConstructors :: TypeInformation -> JValue
jsTypeConstructors (TypeConstructors cs) = JArray (map JString cs)

jsTypeDefinition :: TypeInformation -> JValue
jsTypeDefinition (TypeDefinition d) = JString $ show d

instance JPretty TypeInformation where
    jpretty (TypeName n)            = ("typeName", JString n)
    jpretty (TypeDocumentation d)   = ("documentation", JString $ show d)
    jpretty (TypeConstructors cs)   = ("constructors", JArray (map JString cs))
    jpretty (TypeDefinition d)      = ("definition", JString $ show d)

-- TYPECLASS

jsTypeclassName :: TypeclassInformation -> JValue
jsTypeclassName (TypeclassName n) = JString n

jsTypeclassDocumentation :: TypeclassInformation -> JValue
jsTypeclassDocumentation (TypeclassDocumentation d) = JString $ show d

jsTypeclassMethods :: TypeclassInformation -> JValue
jsTypeclassMethods (TypeclassMethods signs) = JArray (map JString signs)

jsTypeclassDefinition :: TypeclassInformation -> JValue
jsTypeclassDefinition (TypeclassDefinition d) = JString $ show d

instance JPretty TypeclassInformation where
    jpretty (TypeclassName n)           = ("typeclass", JString n)
    jpretty (TypeclassDocumentation d)  = ("documentation", JString $ show d)
    jpretty (TypeclassMethods signs)    = ("methods", JArray (map JString signs))
    jpretty (TypeclassDefinition d)     = ("definition", JString $ show d)

-- OPERATION

jsOperationName :: OperationInformation -> JValue
jsOperationName (OperationName n) = JString n

jsOperationDocumentation :: OperationInformation -> JValue
jsOperationDocumentation (OperationDocumentation d) = JString $ show d

jsOperationSourceCode :: OperationInformation -> JValue
jsOperationSourceCode (OperationSourceCode d) = JString $ show d

jsOperationSignature :: OperationInformation -> JValue
jsOperationSignature (OperationSignature sign) = JString sign

jsOperationInfix :: OperationInformation -> JValue
jsOperationInfix (OperationInfix i) = JString $ show i

jsOperationPrecedence :: OperationInformation -> JValue
jsOperationPrecedence (OperationPrecedence p) = JString $ show p

jsOperationDeterministic :: OperationInformation -> JValue
jsOperationDeterministic (OperationDeterministic det) = JString $ show det

jsOperationDemandness :: OperationInformation -> JValue
jsOperationDemandness (OperationDemandness dems) = JArray (map JNumber dems)

jsOperationIndeterministic :: OperationInformation -> JValue
jsOperationIndeterministic (OperationIndeterministic b) = if b then JTrue else JFalse

jsOperationSolutionCompleteness :: OperationInformation -> JValue
jsOperationSolutionCompleteness (OperationSolutionCompleteness b) = if b then JTrue else JFalse

jsOperationTermination :: OperationInformation -> JValue
jsOperationTermination (OperationTermination b) = if b then JTrue else JFalse

jsOperationTotallyDefined :: OperationInformation -> JValue
jsOperationTotallyDefined (OperationTotallyDefined b) = if b then JTrue else JFalse


instance JPretty OperationInformation where
    jpretty (OperationName n)                   = ("operation", JString n)
    jpretty (OperationDocumentation d)          = ("documentation", JString $ show d)
    jpretty (OperationSourceCode d)             = ("sourceCode", JString $ show d)
    jpretty (OperationSignature sign)           = ("signature", JString sign)
    jpretty (OperationInfix i)                  = ("infix", JString $ show i)
    jpretty (OperationPrecedence p)             = ("precedence", JString $ show p)
    jpretty (OperationDeterministic det)        = ("deterministic", JString $ show det)
    jpretty (OperationDemandness dems)          = ("demandness", JArray (map JNumber dems))
    jpretty (OperationIndeterministic b)        = ("indeterministic", if b then JTrue else JFalse)
    jpretty (OperationSolutionCompleteness b)   = ("solutionCompletenss", if b then JTrue else JFalse)
    jpretty (OperationTermination b)            = ("termination", if b then JTrue else JFalse)
    jpretty (OperationTotallyDefined b)         = ("totallyDefined", if b then JTrue else JFalse)
