-----------------------------------------------------------------------------------------
--- This modules defines configurations for the different objects and their requests.
-----------------------------------------------------------------------------------------

module CurryInfo.Configuration where

import CurryInfo.Types
import CurryInfo.Generator
import CurryInfo.Printer
import CurryInfo.JConvert
import CurryInfo.Verbosity
import CurryInfo.Reader
import CurryInfo.Paths

import JSON.Data
import JSON.Pretty (ppJSON)
import JSON.Convert

import Data.List (find)

----------------------------

-- PACKAGE

packageConfiguration :: Configuration CurryPackage
packageConfiguration =
  [ registerRequest "package"     "\t\tThe name of the package"                 gPackageName        pPackageName
  , registerRequest "versions"    "\t\tThe versions available of the package"   gPackageVersions    pPackageVersions
  ]

-- VERSION

versionConfiguration :: Configuration CurryVersion
versionConfiguration =
  [ registerRequest "version" "\t\tThe version number of the version"         gVersionVersion         pVersionVersion
  , registerRequest "documentation" "\t\tThe documentation of the version"    gVersionDocumentation   pVersionDocumentation
  , registerRequest "categories" "\t\tThe categories of the version"          gVersionCategories      pVersionCategories
  , registerRequest "modules" "\t\tThe exported modules of the version"       gVersionModules         pVersionModules
  , registerRequest "dependencies" "\t\tThe dependencies of the version"      gVersionDependencies    pVersionDependencies
  ]

-- MODULE

moduleConfiguration :: Configuration CurryModule
moduleConfiguration =
  [ registerRequest "module"              "\t\t\tThe name of the module"                              gModuleName             pModuleName 
  , registerRequest "documentation"       "\t\tReference to the documentation comment of the module"  gModuleDocumentation    pModuleDocumentation
  , registerRequest "sourceCode"          "\t\tReference to the source code of the module"            gModuleSourceCode       pModuleSourceCode
  , registerRequest "cass-unsafemodule"   "\tAnalysis result whether the module is safe"              gModuleUnsafeModule     pModuleUnsafeModule
  , registerRequest "typeclasses"         "\t\tThe exported typeclasses of the module"                gModuleTypeclasses      pModuleTypeclasses
  , registerRequest "types"               "\t\t\tThe exported types of the module"                    gModuleTypes            pModuleTypes
  , registerRequest "operations"          "\t\tThe exported operations of the module"                 gModuleOperations       pModuleOperations
  ]

-- TYPE

typeConfiguration :: Configuration CurryType
typeConfiguration =
  [ registerRequest "typeName"        "\t\tThe name of the type"                                  gTypeName           pTypeName
  , registerRequest "documentation"   "\t\tReference to the documentation comment of the type"    gTypeDocumentation  pTypeDocumentation
  , registerRequest "constructors"    "\t\tThe list of the constructors of the type"              gTypeConstructors   pTypeConstructors
  , registerRequest "definition"      "\t\tReference to the definition of the type"               gTypeDefinition     pTypeDefinition
  ]

-- TYPECLASS

typeclassConfiguration :: Configuration CurryTypeclass
typeclassConfiguration =
  [ registerRequest "typeclass"       "\t\tThe name of the typeclass"                                 gTypeclassName          pTypeclassName
  , registerRequest "documentation"   "\t\tReference to the documentation comment of the typeclass"   gTypeclassDocumentation pTypeclassDocumentation
  , registerRequest "methods"         "\t\tThe list of the methods of the typeclass"                  gTypeclassMethods       pTypeclassMethods
  , registerRequest "definition"      "\t\tReference to the definition of the typeclass"              gTypeclassDefinition    pTypeclassDefinition
  ]

-- OPERATION

operationConfiguration :: Configuration CurryOperation
operationConfiguration =
  [ registerRequest "operation"               "\t\tThe name of the operation"                               gOperationName                pOperationName
  , registerRequest "documentation"           "\t\tReference to documentation comment of the operation"     gOperationDocumentation       pOperationDocumentation
  , registerRequest "definition"              "\t\tReference to definition of the operation"                gOperationSourceCode          pOperationSourceCode
  , registerRequest "signature"               "\t\tThe signature of the operation"                          gOperationSignature           pOperationSignature
  , registerRequest "infix"                   "\t\t\tAssociativity of operation"                            gOperationInfix               pOperationInfix
  , registerRequest "precedence"              "\t\tPrecedence of the operation when used infix"             gOperationPrecedence          pOperationPrecedence
  , registerRequest "cass-deterministic"      "\tAnalysis result: operation deterministic?"                 gOperationCASSDeterministic   pOperationCASSDeterministic
  , registerRequest "cass-demand"             "\t\tAnalysis result: demanded arguments"                     gOperationCASSDemand          pOperationCASSDemand
  , registerRequest "cass-indeterministic"    "\tAnalysis result: operation indeterministic?"               gOperationCASSIndeterministic pOperationCASSIndeterministic
  , registerRequest "cass-solcomplete"        "\tAnalysis result: operation solution complete?"             gOperationCASSSolComplete     pOperationCASSSolComplete
  , registerRequest "cass-terminating"        "\tAnalysis result: operation always terminating?"            gOperationCASSTerminating     pOperationCASSTerminating
  , registerRequest "cass-total"              "\t\tAnalysis result: operation totally defined?"             gOperationCASSTotal           pOperationCASSTotal
  , registerRequest "failfree"                "\t\tVerification result: failing behavior of the operation"  gOperationFailFree            pOperationFailFree
  ]

------------------------------------

type Configuration a = [RegisteredRequest a]

data RegisteredRequest a = RegisteredRequest
  { request :: String
  , description :: String
  , extraction :: (Options -> [(String, JValue)] -> IO (Maybe (JValue, String)))
  , generation :: (Options -> a -> IO (Maybe (JValue, String)))
  }

-- This operation looks up a request with the given string, returning
-- the parts of the request.
-- (name, description, extraction, generation)
lookupRequest :: String -> Configuration a
  -> Maybe (String, String,
            Options -> [(String, JValue)] -> IO (Maybe (JValue, String)),
            Options -> a -> IO (Maybe (JValue, String)))
lookupRequest req conf = do
  rreq <- find (\x -> request x == req) conf
  return (request rreq, description rreq, extraction rreq, generation rreq)

registerRequest :: ConvertJSON b => String -> String
               -> Generator a b -> Printer b -> RegisteredRequest a
registerRequest req desc generator printer =
    RegisteredRequest req desc createExtraction createGeneration
  where
    createExtraction opts infos = do
      printDebugMessage opts $ "Looking for information for request '" ++ req ++ "'..."
      case lookup req infos of
        Nothing -> do
          printDebugMessage opts "Information not found."
          return Nothing
        Just jv -> do
          printDebugMessage opts "Information found."
          printDebugMessage opts "Reading information..."
          case fromJSON jv of
            Nothing -> do
              printDebugMessage opts "Reading failed."
              return Nothing
            Just info -> do
              printDebugMessage opts "Reading succeeded."
              printDebugMessage opts "Creating output..."
              output <- printer opts info
              printDebugMessage opts $ "Finished with (" ++ ppJSON jv ++ ", " ++ output ++ ")."
              return $ Just (jv, output)

    createGeneration opts obj = do
      printDebugMessage opts $ "Generating information for request '" ++ req ++ "'..."
      res <- generator opts obj
      case res of
        Nothing -> do
          printDebugMessage opts "Generating failed."
          return Nothing
        Just info -> do
          printDebugMessage opts "Generating succeeded."
          let jv = toJSON info
          printDebugMessage opts "Creating output..."
          output <- printer opts info
          printDebugMessage opts $ "Finished with (" ++ ppJSON jv ++ ", " ++ output ++ ")."
          return $ Just (jv, output)

-- This operation returns a list of strings, each one being a request of the given
-- configuration with their descriptions.
listRequests :: Configuration a -> [String]
listRequests = map (\r -> request r ++ ":" ++ description r)
