-----------------------------------------------------------------------------------------
--- This modules defines two-way conversions between types and json values.
-----------------------------------------------------------------------------------------

module CurryInfo.JConvert where

import CurryInfo.RequestTypes

import JSON.Convert
import JSON.Data

instance ConvertJSON Infix where
  toJSON Infix  = JString "Infix"
  toJSON InfixL = JString "InfixL"
  toJSON InfixR = JString "InfixR"

  fromJSON jv = case jv of
    JString "Infix"  -> Just Infix
    JString "InfixL" -> Just InfixL
    JString "InfixR" -> Just InfixR
    _                -> Nothing

instance ConvertJSON Reference where
  toJSON (Reference path start end) =
    JObject $ toJObject
      [ ("path", toJSON path), ("start", toJSON start), ("end", toJSON end)]

  fromJSON jv = case jv of
    JObject jobject -> do
      path  <- lookupName "path"  jobject >>= fromJSON
      start <- lookupName "start" jobject >>= fromJSON
      end   <- lookupName "end"   jobject >>= fromJSON
      return (Reference path start end)
    _ -> Nothing

instance ConvertJSON VersionConstraint where
  toJSON = toJSON . show

  fromJSON jv = case jv of
    JString vc -> Just (read vc)
    _          -> Nothing

instance ConvertJSON Dependency where
  toJSON (Dependency pkg dsj) =
    JObject $ toJObject
      [("package", toJSON pkg), ("constraints", toJSONList dsj)]

  fromJSON jv = case jv of
    JObject jobject -> do
      pkg <- lookupName "package"     jobject >>= fromJSON
      dsj <- lookupName "constraints" jobject >>= fromJSONList
      return (Dependency pkg dsj)
    _ -> Nothing
