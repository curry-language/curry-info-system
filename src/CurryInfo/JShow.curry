module CurryInfo.JShow where

import CurryInfo.Types

import JSON.Data
import JSON.Pretty (ppJSON)
import Text.Pretty

-- PACKAGE

jsPackageName :: JShower String
jsPackageName = JString

jsPackageVersions :: JShower [String]
jsPackageVersions vsns = JArray (map JString vsns)

-- VERSION

jsVersionVersion :: JShower String
jsVersionVersion vsn = JString vsn

jsVersionDocumentation :: JShower String
jsVersionDocumentation d = JString d

jsVersionCategories :: JShower [String]
jsVersionCategories cats = JArray (map JString cats)

jsVersionModules :: JShower [String]
jsVersionModules mods = JArray (map JString mods)

jsVersionDependencies :: JShower [Dependency]
jsVersionDependencies deps = JArray (map (JString . show) deps)

-- MODULE

jsModuleName :: JShower String
jsModuleName m = JString m

jsModuleDocumentation :: JShower Reference
jsModuleDocumentation d = JString $ show d

jsModuleSourceCode :: JShower Reference
jsModuleSourceCode s = JString $ show s

jsModuleSafe :: JShower Safe
jsModuleSafe s = JString $ show s

jsModuleTypeclasses :: JShower [String]
jsModuleTypeclasses cs = JArray (map JString cs)

jsModuleTypes :: JShower [String]
jsModuleTypes ts = JArray (map JString ts)

jsModuleOperations :: JShower [String]
jsModuleOperations os = JArray (map JString os )

-- TYPE

jsTypeName :: JShower String
jsTypeName t = JString t

jsTypeDocumentation :: JShower Reference
jsTypeDocumentation d = JString $ show d

jsTypeConstructors :: JShower [String]
jsTypeConstructors cons = JArray (map JString cons)

jsTypeDefinition :: JShower Reference
jsTypeDefinition d = JString $ show d

-- TYPECLASS

jsTypeclassName :: JShower String
jsTypeclassName c = JString c

jsTypeclassDocumentation :: JShower Reference
jsTypeclassDocumentation d = JString $ show d

jsTypeclassMethods :: JShower [String]
jsTypeclassMethods methods = JArray (map JString methods)

jsTypeclassDefinition :: JShower Reference
jsTypeclassDefinition d = JString $ show d

-- OPERATION

jsOperationName :: JShower String
jsOperationName o = JString o

jsOperationDocumentation :: JShower Reference
jsOperationDocumentation d = JString $ show d

jsOperationSourceCode :: JShower Reference
jsOperationSourceCode s = JString $ show s

jsOperationSignature :: JShower Signature
jsOperationSignature sign = JString sign

jsOperationInfix :: JShower (Maybe Infix)
jsOperationInfix i = JString $ show i

jsOperationPrecedence :: JShower (Maybe Precedence)
jsOperationPrecedence p = JString $ show p

jsOperationDeterministic :: JShower Deterministic
jsOperationDeterministic det = JString $ show det

jsOperationDemandness :: JShower Demandness
jsOperationDemandness dems = JArray (map JNumber dems)

jsOperationIndeterministic :: JShower Indeterministic
jsOperationIndeterministic b = if b then JTrue else JFalse

jsOperationSolutionCompleteness :: JShower SolutionCompleteness
jsOperationSolutionCompleteness b = if b then JTrue else JFalse

jsOperationTermination :: JShower Termination
jsOperationTermination b = if b then JTrue else JFalse

jsOperationTotallyDefined :: JShower TotallyDefined
jsOperationTotallyDefined b = if b then JTrue else JFalse
