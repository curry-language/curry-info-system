module CurryAnalysisInfrastructure.JParser where

import CurryAnalysisInfrastructure.Types

import JSON.Data
import Text.Pretty (text)
import Data.List (find)

--- This function takes a json value and returns the parsed list of fields, if every field is parsed successfully.
--- If any field fails to be parsed, Nothing is returned.
jparse :: JParser a => JValue -> Maybe [a]
jparse jv = case jv of
    JObject fields ->
        let parsedFields = map jparseField fields
        in sequence parsedFields
    _ -> Nothing

class JParser a where
    jparseField :: (String, JValue) -> Maybe a

-- PACKAGE

instance JParser PackageInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "Package" -> case fieldvalue of 
            JString name -> return (PackageName name)
            _ -> Nothing
        "Versions" -> case fieldvalue of
            JArray jvsns -> case sequence $ map getString jvsns of
                Just vsns -> return (PackageVersions vsns)
                Nothing -> Nothing
            _ -> Nothing
        _ -> Nothing

-- VERSION

instance JParser VersionInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "Version" -> case fieldvalue of
            JString vsn -> return (VersionVersion vsn)
            _ -> Nothing
        "Documentation" -> case fieldvalue of
            JString doc -> return (VersionDocumentation (text doc))
            _ -> Nothing
        "Categories" -> case fieldvalue of
            JArray jcats -> case sequence $ map getString jcats of
                Just cats -> return (VersionCategories cats)
                Nothing -> Nothing
            _ -> Nothing
        "Modules" -> case fieldvalue of 
            JArray jmods -> case sequence $ map getString jmods of
                Just mods -> return (VersionModules mods)
                Nothing -> Nothing
            _ -> Nothing 
        _ -> Nothing 

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

lookupField :: Eq a => a -> [(a, b)] -> Maybe b
lookupField fieldname fields = fmap snd $ find (\(name, _) -> name == fieldname) fields