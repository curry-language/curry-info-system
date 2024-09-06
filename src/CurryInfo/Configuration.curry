module CurryInfo.Configuration where

import CurryInfo.Types
import CurryInfo.Generator
import CurryInfo.Printer
import CurryInfo.JRead
import CurryInfo.JShow
import CurryInfo.Verbosity
import CurryInfo.Reader
import CurryInfo.Paths

import JSON.Data
import JSON.Pretty (ppJSON)

import Data.List (find)

----------------------------

-- PACKAGE

packageConfiguration :: [RegisteredRequest CurryPackage]
packageConfiguration =
    [ registerRequest "package"     "\t\t\tThe name of the package"                 gPackageName        jrPackageName       jsPackageName       pPackageName
    , registerRequest "versions"    "\t\t\tThe versions available of the package"   gPackageVersions    jrPackageVersions   jsPackageVersions   pPackageVersions
    ]

-- VERSION

versionConfiguration :: [RegisteredRequest CurryVersion]
versionConfiguration =
    [ registerRequest "version" "\t\tThe version number of the version"         gVersionVersion         jrVersionVersion        jsVersionVersion        pVersionVersion
    , registerRequest "documentation" "\t\tThe documentation of the version"    gVersionDocumentation   jrVersionDocumentation  jsVersionDocumentation  pVersionDocumentation
    , registerRequest "categories" "\t\tThe categories of the version"          gVersionCategories      jrVersionCategories     jsVersionCategories     pVersionCategories
    , registerRequest "modules" "\t\tThe exported modules of the version"       gVersionModules         jrVersionModules        jsVersionModules        pVersionModules
    , registerRequest "dependencies" "\t\tThe dependencies of the version"      gVersionDependencies    jrVersionDependencies   jsVersionDependencies   pVersionDependencies
    ]

-- MODULE

moduleConfiguration :: [RegisteredRequest CurryModule]
moduleConfiguration =
    [ registerRequest "module"          "\t\t\tThe name of the module"                              gModuleName             jrModuleName             jsModuleName             pModuleName 
    , registerRequest "documentation"   "\t\tReference to the documentation comment of the module"  gModuleDocumentation    jrModuleDocumentation    jsModuleDocumentation    pModuleDocumentation
    , registerRequest "sourceCode"      "\t\tReference to the source code of the module"            gModuleSourceCode       jrModuleSourceCode       jsModuleSourceCode       pModuleSourceCode
    , registerRequest "safe"            "\t\t\tAnalysis result whether the module is safe"          gModuleSafe             jrModuleSafe             jsModuleSafe             pModuleSafe
    , registerRequest "typeclasses"     "\t\tThe exported typeclasses of the module"                gModuleTypeclasses      jrModuleTypeclasses      jsModuleTypeclasses      pModuleTypeclasses
    , registerRequest "types"           "\t\t\tThe exported types of the module"                    gModuleTypes            jrModuleTypes            jsModuleTypes            pModuleTypes
    , registerRequest "operations"      "\t\tThe exported operations of the module"                 gModuleOperations       jrModuleOperations       jsModuleOperations       pModuleOperations
    ]

-- TYPE

typeConfiguration :: [RegisteredRequest CurryType]
typeConfiguration =
    [ registerRequest "typeName"        "\t\tThe name of the type"                                  gTypeName           jrTypeName           jsTypeName           pTypeName
    , registerRequest "documentation"   "\t\tReference to the documentation comment of the type"    gTypeDocumentation  jrTypeDocumentation  jsTypeDocumentation  pTypeDocumentation
    , registerRequest "constructors"    "\t\tThe list of the constructors of the type"              gTypeConstructors   jrTypeConstructors   jsTypeConstructors   pTypeConstructors
    , registerRequest "definition"      "\t\tReference to the definition of the type"               gTypeDefinition     jrTypeDefinition     jsTypeDefinition     pTypeDefinition
    ]

-- TYPECLASS

typeclassConfiguration :: [RegisteredRequest CurryTypeclass]
typeclassConfiguration =
    [ registerRequest "typeclass"       "\t\tThe name of the typeclass"                                 gTypeclassName          jrTypeclassName          jsTypeclassName          pTypeclassName
    , registerRequest "documentation"   "\t\tReference to the documentation comment of the typeclass"   gTypeclassDocumentation jrTypeclassDocumentation jsTypeclassDocumentation pTypeclassDocumentation
    , registerRequest "methods"         "\t\tThe list of the methods of the typeclass"                  gTypeclassMethods       jrTypeclassMethods       jsTypeclassMethods       pTypeclassMethods
    , registerRequest "definition"      "\t\tReference to the definition of the typeclass"              gTypeclassDefinition    jrTypeclassDefinition    jsTypeclassDefinition    pTypeclassDefinition
    ]

-- OPERATION

operationConfiguration :: [RegisteredRequest CurryOperation]
operationConfiguration =
    [ registerRequest "operation"               "\t\tThe name of the operation"                                                 gOperationName                  jrOperationName                  jsOperationName                  pOperationName
    , registerRequest "documentation"           "\t\tReference to the documentation comment of the operation"                   gOperationDocumentation         jrOperationDocumentation         jsOperationDocumentation         pOperationDocumentation
    , registerRequest "definition"              "\t\tReference to the definition of the operation"                              gOperationSourceCode            jrOperationSourceCode            jsOperationSourceCode            pOperationSourceCode
    , registerRequest "signature"               "\t\tThe signature of the operation"                                            gOperationSignature             jrOperationSignature             jsOperationSignature             pOperationSignature
    , registerRequest "infix"                   "\t\t\tWhether the operation is infix and in what way (Infix, InfixL, InfixR)"  gOperationInfix                 jrOperationInfix                 jsOperationInfix                 pOperationInfix
    , registerRequest "precedence"              "\t\tPrecedence of the operation when used infix"                               gOperationPrecedence            jrOperationPrecedence            jsOperationPrecedence            pOperationPrecedence
    , registerRequest "deterministic"           "\t\tAnalysis result whether the operation is deterministic"                    gOperationDeterministic         jrOperationDeterministic         jsOperationDeterministic         pOperationDeterministic
    , registerRequest "demandness"              "\t\tAnalysis result what arguments are demanded"                               gOperationDemandness            jrOperationDemandness            jsOperationDemandness            pOperationDemandness
    , registerRequest "indeterministic"         "\tAnalysis result whether the operation is indeterministic"                    gOperationIndeterministic       jrOperationIndeterministic       jsOperationIndeterministic       pOperationIndeterministic
    , registerRequest "solutionCompleteness"    "\tAnalysis result whether the operation is solution complete"                  gOperationSolutionCompleteness  jrOperationSolutionCompleteness  jsOperationSolutionCompleteness  pOperationSolutionCompleteness
    , registerRequest "termination"             "\t\tAnalysis result whether the operation is guaranteed to always terminate"   gOperationTermination           jrOperationTermination           jsOperationTermination           pOperationTermination
    , registerRequest "totallyDefined"          "\t\tAnalysis result whether the operation is totally defined"                  gOperationTotallyDefined        jrOperationTotallyDefined        jsOperationTotallyDefined        pOperationTotallyDefined
    ]

------------------------------------

data RegisteredRequest a = RegisteredRequest
    { request :: String
    , description' :: String
    , extraction :: (Options -> [(String, JValue)] -> IO (Maybe (JValue, String)))
    , generation :: (Options -> a -> IO (Maybe (JValue, String)))
    }

lookupRequest :: String -> [RegisteredRequest a] -> Maybe (String, String, (Options -> [(String, JValue)] -> IO (Maybe (JValue, String))), (Options -> a -> IO (Maybe (JValue, String))))
lookupRequest req conf = do
    rreq <- find (\x -> request x == req) conf
    return (request rreq, description' rreq, extraction rreq, generation rreq)

registerRequest :: String -> String -> Generator a b -> JReader b -> JShower b -> Printer b -> RegisteredRequest a
registerRequest req desc generator jreader jshower printer =
    RegisteredRequest req desc (createExtraction req jreader printer) (createGeneration req generator jshower printer)

createExtraction :: String -> JReader b -> Printer b -> (Options -> [(String, JValue)] -> IO (Maybe (JValue, String)))
createExtraction req jreader printer opts infos = do
    printDebugMessage opts $ "Looking for information for request '" ++ req ++ "'..."
    case lookup req infos of
        Nothing -> do
            printDebugMessage opts "Information not found."
            return Nothing
        Just jv -> do
            printDebugMessage opts "Information found."
            printDebugMessage opts "Reading information..."
            case jreader jv of
                Nothing -> do
                    printDebugMessage opts "Reading failed."
                    return Nothing
                Just info -> do
                    printDebugMessage opts "Reading succeeded."
                    printDebugMessage opts "Creating output..."
                    output <- printer opts info
                    printDebugMessage opts $ "Finished with (" ++ ppJSON jv ++ ", " ++ output ++ ")."
                    return $ Just (jv, output)

createGeneration :: String -> Generator a b -> JShower b -> Printer b -> (Options -> a -> IO (Maybe (JValue, String)))
createGeneration req generator jshower printer opts obj = do
    printDebugMessage opts $ "Generating information for request '" ++ req ++ "'..."
    res <- generator opts obj
    case res of
        Nothing -> do
            printDebugMessage opts "Generating failed."
            return Nothing
        Just info -> do
            printDebugMessage opts "Generating succeeded."
            let jv = jshower info
            printDebugMessage opts "Creating output..."
            output <- printer opts info
            printDebugMessage opts $ "Finished with (" ++ ppJSON jv ++ ", " ++ output ++ ")."
            return $ Just (jv, output)
