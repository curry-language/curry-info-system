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
        "package" -> PackageName <$> getString fieldvalue
        "versions" -> PackageVersions <$> (join $ mapM getString <$> getArray fieldvalue)
        _ -> Nothing

-- VERSION

instance JParser VersionInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "version" -> VersionVersion <$> getString fieldvalue
        "documentation" -> (VersionDocumentation . text) <$> getString fieldvalue
        "categories" -> VersionCategories <$> (join $ mapM getString <$> getArray fieldvalue)
        "modules" -> VersionModules <$> (join $ mapM getString <$> getArray fieldvalue)
        _ -> Nothing 

-- MODULE

instance JParser ModuleInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "module" -> ModuleName <$> getString fieldvalue
        "documentation" -> (ModuleDocumentation . text) <$> getString fieldvalue
        "sourceCode" -> (ModuleSourceCode . text) <$> getString fieldvalue
        "safe" -> ModuleSafe <$> getSafe fieldvalue
        "exports" -> ModuleExports <$> (join $ mapM getString <$> getArray fieldvalue)
        "typeclasses" -> ModuleTypeclasses <$> (join $ mapM getString <$> getArray fieldvalue)
        "types" -> ModuleTypes <$> (join $ mapM getString <$> getArray fieldvalue)
        "operations" -> ModuleOperations <$> (join $ mapM getString <$> getArray fieldvalue)
        _ -> Nothing

-- TYPE

instance JParser TypeInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "typeName" -> TypeName <$> getString fieldvalue
        "documentation" -> (TypeDocumentation . text) <$> getString fieldvalue
        "constructors" -> TypeConstructors <$> (read <$> getString fieldvalue)
        "definition" -> (TypeDefinition . text) <$> getString fieldvalue

------------------------------------------------------------------------

--- This function converts a string json value into a regular string.
--- If the given value is not a string, Nothing is returned.
getString :: JValue -> Maybe String
getString jv = case jv of
    JString s -> Just s
    _ -> Nothing

--- This function converts a boolean json value into a regular boolean.
--- If the given value is not a boolean, Nothing is returned.
getBool :: JValue -> Maybe Bool
getBool jv = case jv of 
    JTrue -> Just True
    JFalse -> Just False
    _ -> Nothing

--- This function converts a number json value into a regular float.
--- If the given value is not a number, Nothing is returned.
getNumber :: JValue -> Maybe Float
getNumber jv = case jv of 
    JNumber n -> Just n
    _ -> Nothing

getArray :: JValue -> Maybe [JValue]
getArray jv = case jv of
    JArray x -> Just x
    _ -> Nothing

getFields :: JValue -> Maybe [Field]
getFields jv = case jv of
    JObject x -> Just x
    _ -> Nothing

getSafe :: JValue -> Maybe Safe
getSafe jv = case jv of
    JString s -> return $ read s
    _ -> Nothing

lookupField :: Eq a => a -> [(a, b)] -> Maybe b
lookupField fieldname fields = fmap snd $ find (\(name, _) -> name == fieldname) fields