module CurryAnalysisInfrastructure.JParser where

import CurryAnalysisInfrastructure.Types

import JSON.Data
import Text.Pretty (text)
import Data.List (find)
import Control.Monad (join)

type Field = (String, JValue)

--- This function takes a json value and returns the parsed list of fields, if every field is parsed successfully.
--- If any field fails to be parsed, Nothing is returned.
jparse :: JParser a => JValue -> Maybe [a]
jparse jv = join $ mapM jparseField <$> getFields jv


class JParser a where
    jparseField :: Field -> Maybe a

-- PACKAGE

instance JParser PackageInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "package"   -> PackageName <$> getString fieldvalue
        "versions"  -> PackageVersions <$> (join $ mapM getString <$> getArray fieldvalue)
        _           -> Nothing

-- VERSION

instance JParser VersionInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "version"       -> VersionVersion <$> getString fieldvalue
        "documentation" -> (VersionDocumentation . text) <$> getString fieldvalue
        "categories"    -> VersionCategories <$> (join $ mapM getString <$> getArray fieldvalue)
        "modules"       -> VersionModules <$> (join $ mapM getString <$> getArray fieldvalue)
        _               -> Nothing 

-- MODULE

instance JParser ModuleInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "module"        -> ModuleName <$> getString fieldvalue
        "documentation" -> (ModuleDocumentation . text) <$> getString fieldvalue
        "sourceCode"    -> (ModuleSourceCode . text) <$> getString fieldvalue
        "safe"          -> (ModuleSafe . read) <$> getString fieldvalue
        "exports"       -> ModuleExports <$> (join $ mapM getString <$> getArray fieldvalue)
        "typeclasses"   -> ModuleTypeclasses <$> (join $ mapM getString <$> getArray fieldvalue)
        "types"         -> ModuleTypes <$> (join $ mapM getString <$> getArray fieldvalue)
        "operations"    -> ModuleOperations <$> (join $ mapM getString <$> getArray fieldvalue)
        _               -> Nothing

-- TYPE

instance JParser TypeInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "typeName"      -> TypeName <$> getString fieldvalue
        "documentation" -> (TypeDocumentation . text) <$> getString fieldvalue
        "constructors"  -> TypeConstructors <$> (join $ mapM getString <$> getArray fieldvalue)
        "definition"    -> (TypeDefinition . text) <$> getString fieldvalue
        _               -> Nothing

-- TYPECLASS

instance JParser TypeclassInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "typeclass"     -> TypeclassName <$> getString fieldvalue
        "documentation" -> (TypeclassDocumentation . text) <$> getString fieldvalue
        "methods"       -> TypeclassMethods <$> (read <$> getString fieldvalue)
        "definition"    -> (TypeclassDefinition . text) <$> getString fieldvalue
        _               -> Nothing

-- OPERATION

instance JParser OperationInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "operation"             -> OperationName <$> getString fieldvalue
        "documentation"         -> (OperationDocumentation . text) <$> getString fieldvalue
        "sourceCode"            -> (OperationSourceCode . text) <$> getString fieldvalue
        "signature"             -> OperationSignature <$> getString fieldvalue
        "infix"                 -> OperationInfix <$> (read <$> getString fieldvalue)
        "precedence"            -> OperationPrecedence <$> (read <$> getString fieldvalue)
        "deterministic"         -> OperationDeterministic <$> (read <$> getString fieldvalue)
        "demandness"            -> OperationDemandness <$> (read <$> getString fieldvalue)
        "indeterministic"       -> OperationIndeterministic <$> (read <$> getString fieldvalue)
        "solutionCompletenss"   -> OperationSolutionCompleteness <$> (read <$> getString fieldvalue)
        "termination"           -> OperationTermination <$> (read <$> getString fieldvalue)
        "totallyDefined"        -> OperationTotallyDefined <$> (read <$> getString fieldvalue)
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
getFields :: JValue -> Maybe [Field]
getFields jv = case jv of
    JObject x -> Just x
    _ -> Nothing