------------------------------------------------------------------------------
--- This modules defines configurations for the different objects and
--- their requests.
------------------------------------------------------------------------------

module CurryInfo.Configuration where

import Data.List           ( find )

import JSON.Data
import JSON.Pretty         ( ppJSON )
import JSON.Convert

import CurryInfo.Types
import CurryInfo.Generator
import CurryInfo.Printer
import CurryInfo.JConvert
import CurryInfo.Verbosity
import CurryInfo.Reader
import CurryInfo.Paths
import CurryInfo.Helper    ( quote )

----------------------------

-- PACKAGE

packageConfiguration :: Configuration CurryPackage
packageConfiguration =
  [ registerRequest "name"      "Name of the package"               gPackageName     pPackageName
  , registerRequest "versions"  "Available versions of the package" gPackageVersions pPackageVersions
  ]

-- VERSION

versionConfiguration :: Configuration CurryVersion
versionConfiguration =
  [ registerRequest "version"         "Version number of the version"   gVersionVersion         pVersionVersion
  , registerRequest "documentation"   "Documentation of the version"    gVersionDocumentation   pVersionDocumentation
  , registerRequest "categories"      "Categories of the version"       gVersionCategories      pVersionCategories
  , registerRequest "modules"         "All modules of the version"      gVersionModules         pVersionModules
  , registerRequest "exportedmodules" "Exported modules of the version" gVersionExportedModules pVersionModules
  , registerRequest "dependencies"    "Dependencies of the version"     gVersionDependencies    pVersionDependencies
  ]

-- MODULE

moduleConfiguration :: Configuration CurryModule
moduleConfiguration =
  [ registerRequest "name"          "Name of the module"                  gModuleName           pModuleName 
  , registerRequest "documentation" "Documentation comment of the module" gModuleDocumentation  pModuleDocumentation
  , registerRequest "sourcecode"    "Source code of the module"           gModuleSourceCode     pModuleSourceCode
  , registerRequest "unsafe"        "Analysis: is the module unsafe?"     gModuleUnsafeModule   pModuleUnsafeModule
  , registerRequest "classes"       "Exported classes of the module"      gModuleClasses        pModuleClasses
  , registerRequest "types"         "Exported types of the module"        gModuleTypes          pModuleTypes
  , registerRequest "operations"    "Exported operations of the module"   gModuleOperations     pModuleOperations
  ]

-- TYPE

typeConfiguration :: Configuration CurryType
typeConfiguration =
  [ registerRequest "name"          "Name of the type"                         gTypeName           pTypeName
  , registerRequest "documentation" "Documentation comment of the type"        gTypeDocumentation  pTypeDocumentation
  , registerRequest "definition"    "Definition of the type"                   gTypeDefinition     pTypeDefinition
  , registerRequest "constructors"  "The list of the constructors of the type" gTypeConstructors   pTypeConstructors
  ]

-- CLASS

classConfiguration :: Configuration CurryClass
classConfiguration =
  [ registerRequest "name"          "Name of the type class"                    gClassName          pClassName
  , registerRequest "documentation" "Documentation comment of the type class"   gClassDocumentation pClassDocumentation
  , registerRequest "definition"    "Definition of the type class"              gClassDefinition    pClassDefinition
  , registerRequest "methods"       "The list of the methods of the type class" gClassMethods       pClassMethods
  ]

-- OPERATION

operationConfiguration :: Configuration CurryOperation
operationConfiguration =
  [ registerRequest "name"              "The name of the operation"                      gOperationName                pOperationName
  , registerRequest "documentation"     "Documentation comment of the operation"         gOperationDocumentation       pOperationDocumentation
  , registerRequest "definition"        "Definition of the operation"                    gOperationSourceCode          pOperationSourceCode
  , registerRequest "signature"         "Signature of the operation"                     gOperationSignature           pOperationSignature
  , registerRequest "infix"             "Associativity of operation"                     gOperationInfix               pOperationInfix
  , registerRequest "precedence"        "Precedence of the operation when used infix"    gOperationPrecedence          pOperationPrecedence
  , registerRequest "deterministic"     "Analysis: deterministic operation?"             gOperationCASSDeterministic   pOperationCASSDeterministic
  , registerRequest "demand"            "Analysis: demanded arguments"                   gOperationCASSDemand          pOperationCASSDemand
  , registerRequest "indeterministic"   "Analysis: indeterministic operation?"           gOperationCASSIndeterministic pOperationCASSIndeterministic
  , registerRequest "solution-complete" "Analysis: solution complete operation?"         gOperationCASSSolComplete     pOperationCASSSolComplete
  , registerRequest "terminating"       "Analysis: operation always terminating?"        gOperationCASSTerminating     pOperationCASSTerminating
  , registerRequest "totally-defined"   "Analysis: operation totally defined?"           gOperationCASSTotal           pOperationCASSTotal
  , registerRequest "result-values"     "Analysis: result values (top constructors)"     gOperationCASSValues          pOperationCASSValues
  , registerRequest "failfree"          "Verification: non-fail conditions on arguments" gOperationFailFree            pOperationFailFree
  , registerRequest "iotype"            "Inference: in/out type of the operation"        gOperationIOType              pOperationIOType  
  ]

------------------------------------------------------------------------------
-- Definition of types and operations used in the configuration specification.

--- A configuration for some kind of entity consists of a list of registered
--- requests.
type Configuration a = [RegisteredRequest a]

--- Maps a configuration into the list of requests names and descriptions.
confReqs :: Configuration _ -> [(String,String)]
confReqs conf = map (\r -> (request r, description r)) conf

--- A registered requests consists of a request name, description, and
--- operations to print and generate the data of the request.
data RegisteredRequest a = RegisteredRequest
  { request     :: String
  , description :: String
  , extraction  :: (Options -> [(String, JValue)] -> IO (Maybe (JValue,String)))
  , generation  :: (Options -> a -> IO (Maybe (JValue, String)))
  }

-- This operation looks up a request with the given string, returning
-- the parts of the request.
-- The result components are the name, description, extractor, and generator.
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
listRequests = map showReq
 where
  showReq r =
    let req = request r
    in req ++ ":" ++ take (19 - length req) (repeat ' ') ++ description r

--- Returns for a given `QueryObject` the list of corresponding request
--- names and descriptions.
requestsOfQueryObject :: QueryObject -> [(String,String)]
requestsOfQueryObject (QueryPackage _)         = confReqs packageConfiguration
requestsOfQueryObject (QueryVersion _ _)       = confReqs versionConfiguration
requestsOfQueryObject (QueryModule  _ _ _)     = confReqs moduleConfiguration
requestsOfQueryObject (QueryType _ _ _ _)      = confReqs typeConfiguration
requestsOfQueryObject (QueryClass _ _ _ _)     = confReqs classConfiguration
requestsOfQueryObject (QueryOperation _ _ _ _) = confReqs operationConfiguration

------------------------------------------------------------------------------
