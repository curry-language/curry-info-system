------------------------------------------------------------------------------
--- This modules defines configurations for the different objects and
--- their requests.
------------------------------------------------------------------------------

module CurryInfo.Configuration where

import CurryInfo.Types
import CurryInfo.Generator
import CurryInfo.Printer
import CurryInfo.JConvert
import CurryInfo.Verbosity
import CurryInfo.Reader
import CurryInfo.Paths
import CurryInfo.Helper    ( quote )

import JSON.Data
import JSON.Pretty (ppJSON)
import JSON.Convert

import Data.List (find)

----------------------------

-- PACKAGE

packageConfiguration :: Configuration CurryPackage
packageConfiguration =
  [ registerRequest "name"      "\t\t\tName of the package"               gPackageName        pPackageName
  , registerRequest "versions"  "\t\tAvailable versions of the package"   gPackageVersions    pPackageVersions
  ]

-- VERSION

versionConfiguration :: Configuration CurryVersion
versionConfiguration =
  [ registerRequest "version"       "\t\tVersion number of the version"   gVersionVersion         pVersionVersion
  , registerRequest "documentation" "\t\tDocumentation of the version"    gVersionDocumentation   pVersionDocumentation
  , registerRequest "categories"    "\t\tCategories of the version"       gVersionCategories      pVersionCategories
  , registerRequest "modules"       "\t\tExported modules of the version" gVersionModules         pVersionModules
  , registerRequest "dependencies"  "\t\tDependencies of the version"     gVersionDependencies    pVersionDependencies
  ]

-- MODULE

moduleConfiguration :: Configuration CurryModule
moduleConfiguration =
  [ registerRequest "name"              "\t\t\tName of the module"                gModuleName           pModuleName 
  , registerRequest "documentation"     "\t\tDocumentation comment of the module" gModuleDocumentation  pModuleDocumentation
  , registerRequest "sourcecode"        "\t\tSource code of the module"           gModuleSourceCode     pModuleSourceCode
  , registerRequest "cass-unsafemodule" "\tAnalysis: is the module is safe"       gModuleUnsafeModule   pModuleUnsafeModule
  , registerRequest "classes"           "\t\tExported classes of the module"      gModuleClasses        pModuleClasses
  , registerRequest "types"             "\t\t\tExported types of the module"      gModuleTypes          pModuleTypes
  , registerRequest "operations"        "\t\tExported operations of the module"   gModuleOperations     pModuleOperations
  ]

-- TYPE

typeConfiguration :: Configuration CurryType
typeConfiguration =
  [ registerRequest "name"          "\t\t\tName of the type"                       gTypeName           pTypeName
  , registerRequest "documentation" "\t\tDocumentation comment of the type"        gTypeDocumentation  pTypeDocumentation
  , registerRequest "constructors"  "\t\tThe list of the constructors of the type" gTypeConstructors   pTypeConstructors
  , registerRequest "definition"    "\t\tDefinition of the type"                   gTypeDefinition     pTypeDefinition
  ]

-- CLASS

classConfiguration :: Configuration CurryClass
classConfiguration =
  [ registerRequest "name"          "\t\t\tName of the type class"                   gClassName          pClassName
  , registerRequest "documentation" "\t\tDocumentation comment of the type class"    gClassDocumentation pClassDocumentation
  , registerRequest "methods"       "\t\tThe list of the methods of the type class"  gClassMethods       pClassMethods
  , registerRequest "definition"    "\t\tDefinition of the type class"               gClassDefinition    pClassDefinition
  ]

-- OPERATION

operationConfiguration :: Configuration CurryOperation
operationConfiguration =
  [ registerRequest "name"                 "\t\t\tThe name of the operation"                      gOperationName                pOperationName
  , registerRequest "documentation"        "\t\tDocumentation comment of the operation"           gOperationDocumentation       pOperationDocumentation
  , registerRequest "definition"           "\t\tDefinition of the operation"                      gOperationSourceCode          pOperationSourceCode
  , registerRequest "signature"            "\t\tSignature of the operation"                       gOperationSignature           pOperationSignature
  , registerRequest "infix"                "\t\t\tAssociativity of operation"                     gOperationInfix               pOperationInfix
  , registerRequest "precedence"           "\t\tPrecedence of the operation when used infix"      gOperationPrecedence          pOperationPrecedence
  , registerRequest "cass-deterministic"   "\tAnalysis: operation deterministic?"                 gOperationCASSDeterministic   pOperationCASSDeterministic
  , registerRequest "cass-demand"          "\t\tAnalysis: demanded arguments"                     gOperationCASSDemand          pOperationCASSDemand
  , registerRequest "cass-indeterministic" "\tAnalysis: operation indeterministic?"               gOperationCASSIndeterministic pOperationCASSIndeterministic
  , registerRequest "cass-solcomplete"     "\tAnalysis: operation solution complete?"             gOperationCASSSolComplete     pOperationCASSSolComplete
  , registerRequest "cass-terminating"     "\tAnalysis: operation always terminating?"            gOperationCASSTerminating     pOperationCASSTerminating
  , registerRequest "cass-total"           "\t\tAnalysis: operation totally defined?"             gOperationCASSTotal           pOperationCASSTotal
  , registerRequest "cass-values"          "\t\tAnalysis: result values (top construcotrs)"       gOperationCASSValues          pOperationCASSValues
  , registerRequest "failfree"             "\t\tVerification: non-fail conditions on arguments"   gOperationFailFree            pOperationFailFree
  ]

------------------------------------------------------------------------------
-- Definition of types and operations used in the configuration specification.

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

--- Operation to create a `RegisteredRequest` from a generator and a printer.
--- The first argument is the name of the request which is used
--- as an request identifier when CurryInfo is invoked.
--- The second argument is a short description of the request.
--- The further arguments are operations to generate and print request
--- information of type `b`. In order to persistently store this information
--- CurryInfo, `b` must be convertible to JSON.
registerRequest :: ConvertJSON b => String -> String
                -> Generator a b -> Printer b -> RegisteredRequest a
registerRequest req desc generator printer =
  RegisteredRequest req desc createExtraction createGeneration
 where
  createExtraction opts infos = do
    printDebugMessage opts $
      "Looking for information for request " ++ quote req ++ "..."
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
            printDebugMessage opts $
              "Finished with (" ++ ppJSON jv ++ ", " ++ output ++ ")."
            return $ Just (jv, output)

  createGeneration opts obj = do
    let genmsg = "Generating information for request " ++ quote req
    printDebugMessage opts $ genmsg ++ "..."
    res <- generator opts obj
    case res of
      Nothing -> do
        printDebugMessage opts $ genmsg ++ " failed!"
        return Nothing
      Just info -> do
        printDebugMessage opts $ genmsg ++ " succeeded."
        let jv = toJSON info
        printDebugMessage opts $
          "Creating output for request " ++ quote req ++ "..."
        output <- printer opts info
        printDebugMessage opts $
          "Finished with (" ++ ppJSON jv ++ ", " ++ output ++ ")."
        return $ Just (jv, output)

-- This operation returns a list of strings, each one being a request
-- of the given configuration with their descriptions.
listRequests :: Configuration a -> [String]
listRequests = map (\r -> request r ++ ":" ++ description r)
