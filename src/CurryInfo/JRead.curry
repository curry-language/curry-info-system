module CurryInfo.JRead where

import CurryInfo.Types

import JSON.Data

import Text.Pretty (text)

import Data.List (find)
import Data.Maybe (catMaybes)

import Control.Monad (join)

-- PACKAGE

jrPackageName :: JReader String
jrPackageName jv = getString jv

jrPackageVersions :: JReader [String]
jrPackageVersions jv = (join $ mapM getString <$> getArray jv)

-- VERSION

jrVersionVersion :: JReader String
jrVersionVersion jv = getString jv

jrVersionDocumentation :: JReader String
jrVersionDocumentation jv = getString jv

jrVersionCategories :: JReader [String]
jrVersionCategories jv = (join $ mapM getString <$> getArray jv)

jrVersionModules :: JReader [String]
jrVersionModules jv = (join $ mapM getString <$> getArray jv)

jrVersionDependencies :: JReader [Dependency]
jrVersionDependencies jv = do
    arr <- getArray jv
    deps <- mapM getString arr
    return (map read deps)

-- MODULE

jrModuleName :: JReader String
jrModuleName jv = getString jv

jrModuleDocumentation :: JReader Reference
jrModuleDocumentation jv = read <$> getString jv

jrModuleSourceCode :: JReader Reference
jrModuleSourceCode jv = read <$> getString jv

jrModuleSafe :: JReader Safe
jrModuleSafe jv = read <$> getString jv

jrModuleTypeclasses :: JReader [String]
jrModuleTypeclasses jv = (join $ mapM getString <$> getArray jv)

jrModuleTypes :: JReader [String]
jrModuleTypes jv = (join $ mapM getString <$> getArray jv)

jrModuleOperations :: JReader [String]
jrModuleOperations jv = (join $ mapM getString <$> getArray jv)

-- TYPE

jrTypeName :: JReader String
jrTypeName jv = getString jv

jrTypeDocumentation :: JReader Reference
jrTypeDocumentation jv = read <$> getString jv

jrTypeConstructors :: JReader [String]
jrTypeConstructors jv = (join $ mapM getString <$> getArray jv)

jrTypeDefinition :: JReader Reference
jrTypeDefinition jv = read <$> getString jv

-- TYPECLASS

jrTypeclassName :: JReader String
jrTypeclassName jv = getString jv

jrTypeclassDocumentation :: JReader Reference
jrTypeclassDocumentation jv = read <$> getString jv

jrTypeclassMethods :: JReader [String]
jrTypeclassMethods jv = (read <$> getString jv)

jrTypeclassDefinition :: JReader Reference
jrTypeclassDefinition jv = read <$> getString jv

-- OPERATION

jrOperationName :: JReader String
jrOperationName jv = getString jv

jrOperationDocumentation :: JReader Reference
jrOperationDocumentation jv = read <$> getString jv

jrOperationSourceCode :: JReader Reference
jrOperationSourceCode jv = read <$> getString jv

jrOperationSignature :: JReader Signature
jrOperationSignature jv = getString jv

jrOperationInfix :: JReader (Maybe Infix)
jrOperationInfix jv = (read <$> getString jv)

jrOperationPrecedence :: JReader (Maybe Precedence)
jrOperationPrecedence jv = (read <$> getString jv)

jrOperationDeterministic :: JReader Deterministic
jrOperationDeterministic jv = (read <$> getString jv)

jrOperationDemandness :: JReader Demandness
jrOperationDemandness jv = do
    arr <- getArray jv
    numbers <- mapM getNumber arr
    return (map round numbers)

jrOperationIndeterministic :: JReader Indeterministic
jrOperationIndeterministic jv =(getBool jv)

jrOperationSolutionCompleteness :: JReader SolutionCompleteness
jrOperationSolutionCompleteness jv = (getBool jv)

jrOperationTermination :: JReader Termination
jrOperationTermination jv = (getBool jv)

jrOperationTotallyDefined :: JReader TotallyDefined
jrOperationTotallyDefined jv = (getBool jv)

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