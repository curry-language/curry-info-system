module CurryInfo.JParser where

import CurryInfo.Types

import JSON.Data

import Text.Pretty (text)

import Data.List (find)
import Data.Maybe (catMaybes)

import Control.Monad (join)

--- This function takes a json value and returns the parsed list of fields, if every field is parsed successfully.
--- If any field fails to be parsed, Nothing is returned.
jparse :: JParser a => JValue -> Maybe [a]
jparse jv = do
    fields <- getFields jv
    let parsedResults = map jparseField fields
    let results = catMaybes parsedResults
    return results

class JParser a where
    jparseField :: JField -> Maybe a

-- PACKAGE

jrPackageName :: JValue -> Maybe PackageInformation
jrPackageName jv = PackageName <$> getString jv

jrPackageVersions :: JValue -> Maybe PackageInformation
jrPackageVersions jv = PackageVersions <$> (join $ mapM getString <$> getArray jv)

instance JParser PackageInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "package"   -> PackageName <$> getString fieldvalue
        "versions"  -> PackageVersions <$> (join $ mapM getString <$> getArray fieldvalue)
        _           -> Nothing

-- VERSION

jrVersionVersion :: JValue -> Maybe VersionInformation
jrVersionVersion jv = VersionVersion <$> getString jv

jrVersionDocumentation :: JValue -> Maybe VersionInformation
jrVersionDocumentation jv = VersionDocumentation <$> getString jv

jrVersionCategories :: JValue -> Maybe VersionInformation
jrVersionCategories jv = VersionCategories <$> (join $ mapM getString <$> getArray jv)

jrVersionModules :: JValue -> Maybe VersionInformation
jrVersionModules jv = VersionModules <$> (join $ mapM getString <$> getArray jv)

jrVersionDependencies :: JValue -> Maybe VersionInformation
jrVersionDependencies jv = VersionDependencies <$> do
    arr <- getArray jv
    deps <- mapM getString arr
    return (map read deps)

instance JParser VersionInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "version"       -> VersionVersion <$> getString fieldvalue
        "documentation" -> VersionDocumentation <$> getString fieldvalue
        "categories"    -> VersionCategories <$> (join $ mapM getString <$> getArray fieldvalue)
        "modules"       -> VersionModules <$> (join $ mapM getString <$> getArray fieldvalue)
        "dependencies"  -> VersionDependencies <$> do
            arr <- getArray fieldvalue
            deps <- mapM getString arr
            return (map read deps)
        _               -> Nothing

-- MODULE

jrModuleName :: JValue -> Maybe ModuleInformation
jrModuleName jv = ModuleName <$> getString jv

jrModuleDocumentation :: JValue -> Maybe ModuleInformation
jrModuleDocumentation jv = (ModuleDocumentation . read) <$> getString jv

jrModuleSourceCode :: JValue -> Maybe ModuleInformation
jrModuleSourceCode jv = (ModuleSourceCode . read) <$> getString jv

jrModuleSafe :: JValue -> Maybe ModuleInformation
jrModuleSafe jv = (ModuleSafe . read) <$> getString jv

jrModuleTypeclasses :: JValue -> Maybe ModuleInformation
jrModuleTypeclasses jv = ModuleTypeclasses <$> (join $ mapM getString <$> getArray jv)

jrModuleTypes :: JValue -> Maybe ModuleInformation
jrModuleTypes jv = ModuleTypes <$> (join $ mapM getString <$> getArray jv)

jrModuleOperations :: JValue -> Maybe ModuleInformation
jrModuleOperations jv = ModuleOperations <$> (join $ mapM getString <$> getArray jv)

instance JParser ModuleInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "module"        -> ModuleName <$> getString fieldvalue
        "documentation" -> (ModuleDocumentation . read) <$> getString fieldvalue
        "sourceCode"    -> (ModuleSourceCode . read) <$> getString fieldvalue
        "safe"          -> (ModuleSafe . read) <$> getString fieldvalue
        "exports"       -> ModuleExports <$> (join $ mapM getString <$> getArray fieldvalue)
        "typeclasses"   -> ModuleTypeclasses <$> (join $ mapM getString <$> getArray fieldvalue)
        "types"         -> ModuleTypes <$> (join $ mapM getString <$> getArray fieldvalue)
        "operations"    -> ModuleOperations <$> (join $ mapM getString <$> getArray fieldvalue)
        _               -> Nothing

-- TYPE

jrTypeName :: JValue -> Maybe TypeInformation
jrTypeName jv = TypeName <$> getString jv

jrTypeDocumentation :: JValue -> Maybe TypeInformation
jrTypeDocumentation jv = (TypeDocumentation . read) <$> getString jv

jrTypeConstructors :: JValue -> Maybe TypeInformation
jrTypeConstructors jv = TypeConstructors <$> (join $ mapM getString <$> getArray jv)

jrTypeDefinition :: JValue -> Maybe TypeInformation
jrTypeDefinition jv = (TypeDefinition . read) <$> getString jv

instance JParser TypeInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "typeName"      -> TypeName <$> getString fieldvalue
        "documentation" -> (TypeDocumentation . read) <$> getString fieldvalue
        "constructors"  -> TypeConstructors <$> (join $ mapM getString <$> getArray fieldvalue)
        "definition"    -> (TypeDefinition . read) <$> getString fieldvalue
        _               -> Nothing

-- TYPECLASS

jrTypeclassName :: JValue -> Maybe TypeclassInformation
jrTypeclassName jv = TypeclassName <$> getString jv

jrTypeclassDocumentation :: JValue -> Maybe TypeclassInformation
jrTypeclassDocumentation jv = (TypeclassDocumentation . read) <$> getString jv

jrTypeclassMethods :: JValue -> Maybe TypeclassInformation
jrTypeclassMethods jv = TypeclassMethods <$> (read <$> getString jv)

jrTypeclassDefinition :: JValue -> Maybe TypeclassInformation
jrTypeclassDefinition jv = (TypeclassDefinition . read) <$> getString jv

instance JParser TypeclassInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "typeclass"     -> TypeclassName <$> getString fieldvalue
        "documentation" -> (TypeclassDocumentation . read) <$> getString fieldvalue
        "methods"       -> TypeclassMethods <$> (read <$> getString fieldvalue)
        "definition"    -> (TypeclassDefinition . read) <$> getString fieldvalue
        _               -> Nothing

-- OPERATION

jrOperationName :: JValue -> Maybe OperationInformation
jrOperationName jv = OperationName <$> getString jv

jrOperationDocumentation :: JValue -> Maybe OperationInformation
jrOperationDocumentation jv = (OperationDocumentation . read) <$> getString jv

jrOperationSourceCode :: JValue -> Maybe OperationInformation
jrOperationSourceCode jv = (OperationSourceCode . read) <$> getString jv

jrOperationSignature :: JValue -> Maybe OperationInformation
jrOperationSignature jv = OperationSignature <$> getString jv

jrOperationInfix :: JValue -> Maybe OperationInformation
jrOperationInfix jv = OperationInfix <$> (read <$> getString jv)

jrOperationPrecedence :: JValue -> Maybe OperationInformation
jrOperationPrecedence jv = OperationPrecedence <$> (read <$> getString jv)

jrOperationDeterministic :: JValue -> Maybe OperationInformation
jrOperationDeterministic jv = OperationDeterministic <$> (read <$> getString jv)

jrOperationDemandness :: JValue -> Maybe OperationInformation
jrOperationDemandness jv = OperationDemandness <$> do
    arr <- getArray jv
    numbers <- mapM getNumber arr
    return numbers

jrOperationIndeterministic :: JValue -> Maybe OperationInformation
jrOperationIndeterministic jv = OperationIndeterministic <$> (getBool jv)

jrOperationSolutionCompleteness :: JValue -> Maybe OperationInformation
jrOperationSolutionCompleteness jv = OperationSolutionCompleteness <$> (getBool jv)

jrOperationTermination :: JValue -> Maybe OperationInformation
jrOperationTermination jv = OperationTermination <$> (getBool jv)

jrOperationTotallyDefined :: JValue -> Maybe OperationInformation
jrOperationTotallyDefined jv = OperationTotallyDefined <$> (getBool jv)

instance JParser OperationInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "operation"             -> OperationName <$> getString fieldvalue
        "documentation"         -> (OperationDocumentation . read) <$> getString fieldvalue
        "sourceCode"            -> (OperationSourceCode . read) <$> getString fieldvalue
        "signature"             -> OperationSignature <$> getString fieldvalue
        "infix"                 -> OperationInfix <$> (read <$> getString fieldvalue)
        "precedence"            -> OperationPrecedence <$> (read <$> getString fieldvalue)
        "deterministic"         -> OperationDeterministic <$> (read <$> getString fieldvalue)
        "demandness"            -> OperationDemandness <$> do
            arr <- getArray fieldvalue
            numbers <- mapM getNumber arr
            return numbers
        "indeterministic"       -> OperationIndeterministic <$> (getBool fieldvalue)
        "solutionCompletenss"   -> OperationSolutionCompleteness <$> (getBool fieldvalue)
        "termination"           -> OperationTermination <$> (getBool fieldvalue)
        "totallyDefined"        -> OperationTotallyDefined <$> (getBool fieldvalue)
        _                       -> Nothing

------------------------------------------------------------------------

--- This operation converts a string json value into a regular string.
--- If the given value is not a string, Nothing is returned.
getString :: JValue -> Maybe String
getString jv = case jv of
    JString s -> Just s
    _ -> Nothing

--- This operation converts a boolean json value into a regular boolean.
--- If the given value is not a boolean, Nothing is returned.
getBool :: JValue -> Maybe Bool
getBool jv = case jv of 
    JTrue -> Just True
    JFalse -> Just False
    _ -> Nothing

--- This operation converts a number json value into a regular float.
--- If the given value is not a number, Nothing is returned.
getNumber :: JValue -> Maybe Float
getNumber jv = case jv of 
    JNumber n -> Just n
    _ -> Nothing

-- This operation returns the list from an array json value.
-- If the given json value is not an array, Nothing is returned.
getArray :: JValue -> Maybe [JValue]
getArray jv = case jv of
    JArray x -> Just x
    _ -> Nothing

-- This operation returns the list of fields from a json object.
-- If the given json value is not an object, Nothing is returned.
getFields :: JValue -> Maybe [JField]
getFields jv = case jv of
    JObject x -> Just x
    _ -> Nothing

{-
-- This operation returns the dependency entry from a json object.
-- If the given json value is not an object, Nothing is returned.
getDependency :: JValue -> Maybe Dependency
getDependency jv = case jv of
    JObject [("package", JString pkg), ("lowerBound", JString lb), ("upperBound", JString ub)] -> Just (pkg, lb, read ub)
    _ -> Nothing
-}