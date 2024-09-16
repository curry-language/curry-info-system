module CurryInfo.JShow where

import CurryInfo.Types

import JSON.Data
import JSON.Pretty (ppJSON)
import Text.Pretty

-- PACKAGE

jsPackageName :: JShower String
jsPackageName = jsString

jsPackageVersions :: JShower [String]
jsPackageVersions = jsMap jsString

-- VERSION

jsVersionVersion :: JShower String
jsVersionVersion vsn = jsString vsn

jsVersionDocumentation :: JShower String
jsVersionDocumentation = jsString

jsVersionCategories :: JShower [String]
jsVersionCategories = jsMap jsString

jsVersionModules :: JShower [String]
jsVersionModules = jsMap jsString

jsVersionDependencies :: JShower [Dependency]
jsVersionDependencies = jsMap jsShow

-- MODULE

jsModuleName :: JShower String
jsModuleName = jsString

jsModuleDocumentation :: JShower Reference
jsModuleDocumentation = jsShow

jsModuleSourceCode :: JShower Reference
jsModuleSourceCode = jsShow

jsModuleSafe :: JShower Safe
jsModuleSafe = jsShow

jsModuleTypeclasses :: JShower [String]
jsModuleTypeclasses = jsMap jsString

jsModuleTypes :: JShower [String]
jsModuleTypes = jsMap jsString

jsModuleOperations :: JShower [String]
jsModuleOperations = jsMap jsString

-- TYPE

jsTypeName :: JShower String
jsTypeName = jsString

jsTypeDocumentation :: JShower Reference
jsTypeDocumentation = jsShow

jsTypeConstructors :: JShower [String]
jsTypeConstructors = jsMap jsString

jsTypeDefinition :: JShower Reference
jsTypeDefinition = jsShow

-- TYPECLASS

jsTypeclassName :: JShower String
jsTypeclassName = jsString

jsTypeclassDocumentation :: JShower Reference
jsTypeclassDocumentation = jsShow

jsTypeclassMethods :: JShower [String]
jsTypeclassMethods = jsMap jsString

jsTypeclassDefinition :: JShower Reference
jsTypeclassDefinition = jsShow

-- OPERATION

jsOperationName :: JShower String
jsOperationName = jsString

jsOperationDocumentation :: JShower Reference
jsOperationDocumentation = jsShow

jsOperationSourceCode :: JShower Reference
jsOperationSourceCode = jsShow

jsOperationSignature :: JShower Signature
jsOperationSignature = jsString

jsOperationInfix :: JShower (Maybe Infix)
jsOperationInfix = jsShow

jsOperationPrecedence :: JShower (Maybe Precedence)
jsOperationPrecedence = jsShow

jsOperationDeterministic :: JShower Deterministic
jsOperationDeterministic = jsShow

jsOperationDemandness :: JShower Demandness
jsOperationDemandness = jsMap jsNumber

jsOperationIndeterministic :: JShower Indeterministic
jsOperationIndeterministic = jsBool

jsOperationSolutionCompleteness :: JShower SolutionCompleteness
jsOperationSolutionCompleteness = jsBool

jsOperationTermination :: JShower Termination
jsOperationTermination = jsBool

jsOperationTotallyDefined :: JShower TotallyDefined
jsOperationTotallyDefined = jsBool

-- HELPER

jsBool :: Bool -> JValue
jsBool True = JTrue
jsBool False = JFalse

jsString :: String -> JValue
jsString = JString

jsShow :: Show a => a -> JValue
jsShow = JString . show

jsMap :: (a -> JValue) -> [a] -> JValue
jsMap f = JArray . map f

jsNumber :: Real a => a -> JValue
jsNumber = JNumber . toFloat