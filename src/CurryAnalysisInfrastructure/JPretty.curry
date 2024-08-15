module CurryAnalysisInfrastructure.JPretty where

import CurryAnalysisInfrastructure.Types

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

instance JPretty PackageInformation where
    jpretty (PackageName s)         = ("package", JString s)
    jpretty (PackageVersions vsns)  = ("versions", JArray (map JString vsns))

instance JPretty VersionInformation where
    jpretty (VersionVersion vsn)            = ("version", JString vsn)
    jpretty (VersionDocumentation d)        = ("documentation", JString $ pPrint d)
    jpretty (VersionCategories cats)        = ("categories", JArray (map JString cats))
    jpretty (VersionModules mods)           = ("modules", JArray (map JString mods))
    jpretty (VersionDependencies deps)      = ("dependencies", JArray (map f deps))
        where
        f (pkg, lb, ub) = JObject [("package", JString pkg), ("lowerBound", JString (show lb)), ("upperBound", JString (show ub))]

instance JPretty ModuleInformation where
    jpretty (ModuleName m)          = ("module", JString m)
    jpretty (ModuleDocumentation d) = ("documentation", JString $ pPrint d)
    jpretty (ModuleSourceCode d)    = ("sourceCode", JString $ pPrint d)
    jpretty (ModuleSafe s)          = ("safe", JString $ show s)
    jpretty (ModuleExports es)      = ("exports", JArray (map JString es))
    jpretty (ModuleTypeclasses tcs) = ("typeclasses", JArray (map JString tcs))
    jpretty (ModuleTypes ts)        = ("types", JArray (map JString ts))
    jpretty (ModuleOperations ops)  = ("operations", JArray (map JString ops))

instance JPretty TypeInformation where
    jpretty (TypeName n)            = ("typeName", JString n)
    jpretty (TypeDocumentation d)   = ("documentation", JString $ pPrint d)
    jpretty (TypeConstructors cs)   = ("constructors", JArray (map JString cs))
    jpretty (TypeDefinition d)      = ("definition", JString $ pPrint d)

instance JPretty TypeclassInformation where
    jpretty (TypeclassName n)           = ("typeclass", JString n)
    jpretty (TypeclassDocumentation d)  = ("documentation", JString $ pPrint d)
    jpretty (TypeclassMethods signs)    = ("methods", JArray (map JString signs))
    jpretty (TypeclassDefinition d)     = ("definition", JString $ pPrint d)

instance JPretty OperationInformation where
    jpretty (OperationName n)                   = ("operation", JString n)
    jpretty (OperationDocumentation d)          = ("documentation", JString $ pPrint d)
    jpretty (OperationSourceCode d)             = ("sourceCode", JString $ pPrint d)
    jpretty (OperationSignature sign)           = ("signature", JString sign)
    jpretty (OperationInfix i)                  = ("infix", JString $ show i)
    jpretty (OperationPrecedence p)             = ("precedence", JNumber (fromIntegral p))
    jpretty (OperationDeterministic det)        = ("deterministic", JString $ show det)
    jpretty (OperationDemandness dems)          = ("demandness", JArray (map JNumber dems))
    jpretty (OperationIndeterministic b)        = ("indeterministic", if b then JTrue else JFalse)
    jpretty (OperationSolutionCompleteness b)   = ("solutionCompletenss", if b then JTrue else JFalse)
    jpretty (OperationTermination b)            = ("termination", if b then JTrue else JFalse)
    jpretty (OperationTotallyDefined b)         = ("totallyDefined", if b then JTrue else JFalse)
