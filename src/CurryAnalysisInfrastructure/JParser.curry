module CurryAnalysisInfrastructure.JParser where

import CurryAnalysisInfrastructure.Types

import JSON.Data

jparse :: JParser a => JValue -> Maybe [a]
jparse jv = case jv of
    JObject fields ->
        sequence (map jparseField fields)
    _ -> Nothing

class JParser a where
    jparseField :: (String, JValue) -> Maybe a

instance JParser PackageInformation where
    jparseField (fieldname, fieldvalue) = case fieldname of
        "Name" -> case fieldvalue of 
            JString name -> return (PackageName name)
            _ -> Nothing
        "Versions" -> case fieldvalue of
            JArray jvsns -> case sequence $ map getString jvsns of
                Just vsns -> Just (PackageVersions vsns)
                Nothing -> Nothing
            _ -> Nothing
        _ -> Nothing

------------------------------------------------------------------------

getString :: JValue -> Maybe String
getString jv = case jv of
    JString s -> Just s
    _ -> Nothing

getBool :: JValue -> Maybe Bool
getBool jv = case jv of 
    JTrue -> Just True
    JFalse -> Just False
    _ -> Nothing

getNumber :: JValue -> Maybe Float
getNumber jv = case jv of 
    JNumber n -> Just n
    _ -> Nothing