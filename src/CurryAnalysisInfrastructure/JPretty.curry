module CurryAnalysisInfrastructure.JPretty where

import CurryAnalysisInfrastructure.Types

import JSON.Data
import JSON.Pretty (ppJSON)
import Text.Pretty

--- This function takes a list of fields and generates a json object with the given fields.
json :: JPretty a => [a] -> JValue
json infos = JObject (map jpretty infos)

--- This function generates only the json value for a field, excluding the field name.
jsonValue :: JPretty a => a -> JValue
jsonValue = snd . jpretty

jsonOutput :: JPretty a => a -> (String, String)
jsonOutput x =
    let (f, v) = jpretty x in (f, ppJSON v)

class JPretty a where
    jpretty :: a -> (String, JValue)

instance JPretty PackageInformation where
    jpretty (PackageName s) = ("Package", JString s)
    jpretty (PackageVersions vsns) = ("Versions", JArray (map JString vsns))

instance JPretty VersionInformation where
    jpretty (VersionVersion vsn) = ("Version", JString vsn)
    jpretty (VersionDocumentation d) = ("Documentation", JString $ pPrint d)
    jpretty (VersionCategories cats) = ("Categories", JArray (map JString cats))
    jpretty (VersionModules mods) = ("Modules", JArray (map JString mods))

instance JPretty ModuleInformation where
    jpretty (ModuleName m) = ("Module", JString m)
    jpretty (ModuleDocumentation d) = ("Documentation", JString $ pPrint d)
    jpretty (ModuleSourceCode d) = ("SourceCode", JString $ pPrint d)
    jpretty (ModuleSafe b) = ("Safe", if b then JTrue else JFalse)
    jpretty (ModuleExports es) = ("Exports", JArray (map JString es))
    jpretty (ModuleTypeclasses tcs) = ("Typeclasses", JArray (map JString tcs))
    jpretty (ModuleTypes ts) = ("Types", JArray (map JString ts))
    jpretty (ModuleOperations ops) = ("Operations", JArray (map JString ops))

instance JPretty TypeInformation where
    jpretty (TypeDocumentation d) = ("Documentation", JString $ pPrint d)
    jpretty (TypeConstructors e) = case e of 
        Left _ -> ("Constructors", JString "External")
        Right cs -> ("Constructors", JArray (map JString cs))
    jpretty (TypeDefinition d) = ("Definition", JString $ pPrint d)

instance JPretty TypeclassInformation where
    jpretty (TypeclassDocumentation d) = ("Documentation", JString $ pPrint d)
    jpretty (TypeclassMethods signs) = ("Methods", JArray (map JString signs))
    jpretty (TypeclassDefinition d) = ("Definition", JString $ pPrint d)

instance JPretty OperationInformation where
    jpretty (OperationDocumentation d) = ("Documentation", JString $ pPrint d)
    jpretty (OperationSourceCode d) = ("SourceCode", JString $ pPrint d)
    jpretty (OperationSignature sign) = ("Signature", JString sign)
    jpretty (OperationInfix i) = ("Infix", JString $ show i)
    jpretty (OperationPrecedence p) = ("Precedence", JNumber (fromIntegral p))
    jpretty (OperationDeterminism det) = ("Determinism", JString $ show det)
    jpretty (OperationDemandness dems) = ("Demandness", JArray (map (JNumber . fromIntegral) dems))
    jpretty (OperationIndeterminism b) = ("Indeterminism", if b then JTrue else JFalse)
    jpretty (OperationSolutionCompleteness b) = ("SolutionCompletenss", if b then JTrue else JFalse)
    jpretty (OperationTermination b) = ("Termination", if b then JTrue else JFalse)
    jpretty (OperationTotallyDefined b) = ("TotallyDefined", if b then JTrue else JFalse)
