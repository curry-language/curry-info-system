module CurryInfo.Printer where

import CurryInfo.Types
import CurryInfo.Verbosity (printDebugMessage)

import System.Directory (doesFileExist)

--type Printer a = Options -> a -> IO (Maybe String)

slice :: Int -> Int -> [a] -> [a]
slice start end l = take (end - start) (drop start l)

failMessage :: String
failMessage = "FAILED TO PRINT INFORMATION"

printFromReference :: Options -> Reference -> IO String
printFromReference opts (path, start, end) = do
    b <- doesFileExist path
    case b of
        False -> do
            printDebugMessage opts $ "File '" ++ path ++ "' does not exist."
            return "FAILED DUE TO FILE NOT EXISTING"
        True -> do
            printDebugMessage opts $ "Reading from file '" ++ path ++ "'..."
            content <- readFile path
            let ls = lines content
            return (unlines (slice start end ls))

-- PACKAGE

type PackagePrinter = Printer PackageInformation

printPackageName :: PackagePrinter
printPackageName opts info = case info of
    PackageName name -> return (name)
    _ -> return failMessage

printPackageVersions :: PackagePrinter
printPackageVersions opts info = case info of
    PackageVersions vsns -> return (show vsns)
    _ -> return failMessage

-- VERSION

type VersionPrinter = Printer VersionInformation

printVersionVersion :: VersionPrinter
printVersionVersion opts info = case info of
    VersionVersion vsn -> return (vsn)
    _ -> return failMessage

printVersionDocumentation :: VersionPrinter
printVersionDocumentation opts info = case info of
    VersionDocumentation path -> do
        b <- doesFileExist path
        case b of
            False -> do
                printDebugMessage opts $ "File '" ++ path ++ "' does not exist."
                return "FAILED DUE TO FILE NOT EXISTING"
            True -> do
                printDebugMessage opts $ "Reading from file '" ++ path ++ "'..."
                content <- readFile path
                return content
    _ -> return failMessage

printVersionCategories :: VersionPrinter
printVersionCategories opts info = case info of
    VersionCategories cats -> return (show cats)
    _ -> return failMessage

printVersionModules :: VersionPrinter
printVersionModules opts info = case info of
    VersionModules mods -> return (show mods)
    _ -> return failMessage

printVersionDependencies :: VersionPrinter
printVersionDependencies opts info = case info of
    VersionDependencies deps -> return (show deps)
    _ -> return failMessage
    
-- MODULE

type ModulePrinter = Printer ModuleInformation

printModuleName :: ModulePrinter
printModuleName opts info = case info of
    ModuleName name -> return (name)
    _ -> return failMessage

printModuleDocumentation :: ModulePrinter
printModuleDocumentation opts info = case info of
    ModuleDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

printModuleSourceCode :: ModulePrinter
printModuleSourceCode opts info = case info of
    ModuleSourceCode ref -> printFromReference opts ref
    _ -> return failMessage

printModuleSafe :: ModulePrinter
printModuleSafe opts info = case info of
    ModuleSafe safe -> return (show safe)
    _ -> return failMessage

printModuleTypeclasses :: ModulePrinter
printModuleTypeclasses opts info = case info of
    ModuleTypeclasses cs -> return (show cs)
    _ -> return failMessage

printModuleTypes :: ModulePrinter
printModuleTypes opts info = case info of
    ModuleTypes ts -> return (show ts)
    _ -> return failMessage

printModuleOperations :: ModulePrinter
printModuleOperations opts info = case info of
    ModuleOperations os -> return (show os)
    _ -> return failMessage

-- TYPE

type TypePrinter = Printer TypeInformation

printTypeName :: TypePrinter
printTypeName opts info = case info of
    TypeName name -> return (name)
    _ -> return failMessage

printTypeDocumentation :: TypePrinter
printTypeDocumentation opts info = case info of
    TypeDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

printTypeConstructors :: TypePrinter
printTypeConstructors opts info = case info of
    TypeConstructors cons -> return (show cons)
    _ -> return failMessage

printTypeDefinition :: TypePrinter
printTypeDefinition opts info = case info of
    TypeDefinition ref -> printFromReference opts ref
    _ -> return failMessage

-- TYPECLASS

type TypeclassPrinter = Printer TypeclassInformation

printTypeclassName :: TypeclassPrinter
printTypeclassName opts info = case info of
    TypeclassName name -> return (name)
    _ -> return failMessage

printTypeclassDocumentation :: TypeclassPrinter
printTypeclassDocumentation opts info = case info of
    TypeclassDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

printTypeclassMethods :: TypeclassPrinter
printTypeclassMethods opts info = case info of
    TypeclassMethods ms -> return (show ms)
    _ -> return failMessage

printTypeclassDefinition :: TypeclassPrinter
printTypeclassDefinition opts info = case info of
    TypeclassDefinition ref -> printFromReference opts ref
    _ -> return failMessage

-- OPERATION

type OperationPrinter = Printer OperationInformation

printOperationName :: OperationPrinter
printOperationName opts info = case info of
    OperationName name -> return (name)
    _ -> return failMessage

printOperationDocumentation :: OperationPrinter
printOperationDocumentation opts info = case info of
    OperationDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

printOperationSourceCode :: OperationPrinter
printOperationSourceCode opts info = case info of
    OperationSourceCode ref -> printFromReference opts ref
    _ -> return failMessage

printOperationSignature :: OperationPrinter
printOperationSignature opts info = case info of
    OperationSignature s -> return (s)
    _ -> return failMessage

printOperationInfix :: OperationPrinter
printOperationInfix opts info = case info of
    OperationInfix inf -> return (show inf)
    _ -> return failMessage

printOperationPrecedence :: OperationPrinter
printOperationPrecedence opts info = case info of
    OperationPrecedence p -> return (show p)
    _ -> return failMessage

printOperationDeterministic :: OperationPrinter
printOperationDeterministic opts info = case info of
    OperationDeterministic det -> return (show det)
    _ -> return failMessage

printOperationDemandness :: OperationPrinter
printOperationDemandness opts info = case info of
    OperationDemandness dem -> return (show dem)
    _ -> return failMessage

printOperationIndeterministic :: OperationPrinter
printOperationIndeterministic opts info = case info of
    OperationIndeterministic ind -> return (show ind)
    _ -> return failMessage

printOperationSolutionCompleteness :: OperationPrinter
printOperationSolutionCompleteness opts info = case info of
    OperationSolutionCompleteness sol -> return (show sol)
    _ -> return failMessage

printOperationTermination :: OperationPrinter
printOperationTermination opts info = case info of
    OperationTermination t -> return (show t)
    _ -> return failMessage

printOperationTotallyDefined :: OperationPrinter
printOperationTotallyDefined opts info = case info of
    OperationTotallyDefined t -> return (show t)
    _ -> return failMessage
