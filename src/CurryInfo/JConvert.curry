-----------------------------------------------------------------------------------------
--- This modules defines two-way conversions between types and json values.
-----------------------------------------------------------------------------------------

module CurryInfo.JConvert where

import CurryInfo.Types

import JSON.Convert
import JSON.Data

instance ConvertJSON Infix where
    toJSON Infix = JString "Infix"
    toJSON InfixL = JString "InfixL"
    toJSON InfixR = JString "InfixR"

    fromJSON jv = case jv of
        JString "Infix" -> Just Infix
        JString "InfixL" -> Just InfixL
        JString "InfixR" -> Just InfixR
        _ -> Nothing

instance ConvertJSON Reference where
    toJSON (Reference path start end) =
      JObject [ ("path", toJSON path)
              , ("start", toJSON start), ("end", toJSON end)]

    fromJSON jv = case jv of
        JObject fields -> do
            path <- lookup "path" fields >>= fromJSON
            start <- lookup "start" fields >>= fromJSON
            end <- lookup "end" fields >>= fromJSON
            return (Reference path start end)
        _ -> Nothing

instance ConvertJSON VersionConstraint where
    toJSON = toJSON . show

    fromJSON jv = case jv of
        JString vc -> Just (read vc)
        _ -> Nothing

instance ConvertJSON Dependency where
    toJSON (Dependency pkg dsj) =
      JObject [("package", toJSON pkg), ("constraints", toJSONList dsj)]

    fromJSON jv = case jv of
        JObject fields -> do
            pkg <- lookup "package" fields >>= fromJSON
            dsj <- lookup "constraints" fields >>= fromJSONList
            return (Dependency pkg dsj)
        _ -> Nothing
