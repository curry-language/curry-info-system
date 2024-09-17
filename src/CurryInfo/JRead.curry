module CurryInfo.JRead where

import CurryInfo.Types

import JSON.Data

-- PACKAGE

jrPackageName :: JReader String
jrPackageName = jrString

jrPackageVersions :: JReader [String]
jrPackageVersions = jrMap jrString

-- VERSION

jrVersionVersion :: JReader String
jrVersionVersion = jrString

jrVersionDocumentation :: JReader String
jrVersionDocumentation = jrString

jrVersionCategories :: JReader [String]
jrVersionCategories = jrMap jrString

jrVersionModules :: JReader [String]
jrVersionModules = jrMap jrString

jrVersionDependencies :: JReader [Dependency]
jrVersionDependencies = jrMap jrRead

-- MODULE

jrModuleName :: JReader String
jrModuleName = jrString

jrModuleDocumentation :: JReader Reference
jrModuleDocumentation = jrRead

jrModuleSourceCode :: JReader Reference
jrModuleSourceCode = jrRead

jrModuleSafe :: JReader Safe
jrModuleSafe = jrRead

jrModuleTypeclasses :: JReader [String]
jrModuleTypeclasses = jrMap jrString

jrModuleTypes :: JReader [String]
jrModuleTypes = jrMap jrString

jrModuleOperations :: JReader [String]
jrModuleOperations = jrMap jrString

-- TYPE

jrTypeName :: JReader String
jrTypeName = jrString

jrTypeDocumentation :: JReader Reference
jrTypeDocumentation = jrRead

jrTypeConstructors :: JReader [String]
jrTypeConstructors = jrMap jrString

jrTypeDefinition :: JReader Reference
jrTypeDefinition = jrRead

-- TYPECLASS

jrTypeclassName :: JReader String
jrTypeclassName = jrString

jrTypeclassDocumentation :: JReader Reference
jrTypeclassDocumentation = jrRead

jrTypeclassMethods :: JReader [String]
jrTypeclassMethods = jrRead

jrTypeclassDefinition :: JReader Reference
jrTypeclassDefinition = jrRead

-- OPERATION

jrOperationName :: JReader String
jrOperationName = jrString

jrOperationDocumentation :: JReader Reference
jrOperationDocumentation = jrRead

jrOperationSourceCode :: JReader Reference
jrOperationSourceCode = jrRead

jrOperationSignature :: JReader Signature
jrOperationSignature = jrString

jrOperationInfix :: JReader (Maybe Infix)
jrOperationInfix = jrRead

jrOperationPrecedence :: JReader (Maybe Precedence)
jrOperationPrecedence = jrRead

jrOperationDeterministic :: JReader Deterministic
jrOperationDeterministic = jrRead

jrOperationDemandness :: JReader Demandness
jrOperationDemandness = jrMap (fmap round . jrNumber)

jrOperationIndeterministic :: JReader Indeterministic
jrOperationIndeterministic = jrBool

jrOperationSolutionCompleteness :: JReader SolutionCompleteness
jrOperationSolutionCompleteness = jrBool

jrOperationTermination :: JReader Termination
jrOperationTermination = jrBool

jrOperationTotallyDefined :: JReader TotallyDefined
jrOperationTotallyDefined = jrBool

-- HELPER

--- This operation converts a string json value into a regular string.
--- If the given value is not a string, Nothing is returned.
jrString :: JValue -> Maybe String
jrString jv = case jv of
    JString s -> Just s
    _ -> Nothing

--- This operation converts a number json value into a regular float.
--- If the given value is not a number, Nothing is returned.
jrNumber :: JValue -> Maybe Float
jrNumber jv = case jv of
    JNumber n -> Just n
    _ -> Nothing

--- This operation converts a boolean json value into a regular boolean.
--- If the given value is not a boolean, Nothing is returned.
jrBool :: JValue -> Maybe Bool
jrBool jv = case jv of
    JTrue -> Just True
    JFalse -> Just False
    _ -> Nothing

--- This operation converts a string json value into a value with Read instance.
--- If the given value is not a string, Nothing is returned.
jrRead :: Read a => JValue -> Maybe a
jrRead jv = case jv of
    JString s -> Just (read s)
    _ -> Nothing

--- This operation reads a json array  and maps the given function over that.
--- It the given value is not an array or the result of the function for one element
--- is Nothing, Nothing is returned.
jrMap :: (JValue -> Maybe a) -> JValue -> Maybe [a]
jrMap f jv = case jv of
    JArray arr -> mapM f arr
    _ -> Nothing
