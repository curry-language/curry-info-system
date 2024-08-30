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

pPackageName :: PackagePrinter
pPackageName opts info = case info of
    PackageName name -> return (name)
    _ -> return failMessage

pPackageVersions :: PackagePrinter
pPackageVersions opts info = case info of
    PackageVersions vsns -> return (show vsns)
    _ -> return failMessage

-- VERSION

type VersionPrinter = Printer VersionInformation

pVersionVersion :: VersionPrinter
pVersionVersion opts info = case info of
    VersionVersion vsn -> return (vsn)
    _ -> return failMessage

pVersionDocumentation :: VersionPrinter
pVersionDocumentation opts info = case info of
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

pVersionCategories :: VersionPrinter
pVersionCategories opts info = case info of
    VersionCategories cats -> return (show cats)
    _ -> return failMessage

pVersionModules :: VersionPrinter
pVersionModules opts info = case info of
    VersionModules mods -> return (show mods)
    _ -> return failMessage

pVersionDependencies :: VersionPrinter
pVersionDependencies opts info = case info of
    VersionDependencies deps -> return (show deps)
    _ -> return failMessage
    
-- MODULE

type ModulePrinter = Printer ModuleInformation

pModuleName :: ModulePrinter
pModuleName opts info = case info of
    ModuleName name -> return (name)
    _ -> return failMessage

pModuleDocumentation :: ModulePrinter
pModuleDocumentation opts info = case info of
    ModuleDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

pModuleSourceCode :: ModulePrinter
pModuleSourceCode opts info = case info of
    ModuleSourceCode ref -> printFromReference opts ref
    _ -> return failMessage

pModuleSafe :: ModulePrinter
pModuleSafe opts info = case info of
    ModuleSafe safe -> return (show safe)
    _ -> return failMessage

pModuleTypeclasses :: ModulePrinter
pModuleTypeclasses opts info = case info of
    ModuleTypeclasses cs -> return (show cs)
    _ -> return failMessage

pModuleTypes :: ModulePrinter
pModuleTypes opts info = case info of
    ModuleTypes ts -> return (show ts)
    _ -> return failMessage

pModuleOperations :: ModulePrinter
pModuleOperations opts info = case info of
    ModuleOperations os -> return (show os)
    _ -> return failMessage

-- TYPE

type TypePrinter = Printer TypeInformation

pTypeName :: TypePrinter
pTypeName opts info = case info of
    TypeName name -> return (name)
    _ -> return failMessage

pTypeDocumentation :: TypePrinter
pTypeDocumentation opts info = case info of
    TypeDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

pTypeConstructors :: TypePrinter
pTypeConstructors opts info = case info of
    TypeConstructors cons -> return (show cons)
    _ -> return failMessage

pTypeDefinition :: TypePrinter
pTypeDefinition opts info = case info of
    TypeDefinition ref -> printFromReference opts ref
    _ -> return failMessage

-- TYPECLASS

type TypeclassPrinter = Printer TypeclassInformation

pTypeclassName :: TypeclassPrinter
pTypeclassName opts info = case info of
    TypeclassName name -> return (name)
    _ -> return failMessage

pTypeclassDocumentation :: TypeclassPrinter
pTypeclassDocumentation opts info = case info of
    TypeclassDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

pTypeclassMethods :: TypeclassPrinter
pTypeclassMethods opts info = case info of
    TypeclassMethods ms -> return (show ms)
    _ -> return failMessage

pTypeclassDefinition :: TypeclassPrinter
pTypeclassDefinition opts info = case info of
    TypeclassDefinition ref -> printFromReference opts ref
    _ -> return failMessage

-- OPERATION

type OperationPrinter = Printer OperationInformation

pOperationName :: OperationPrinter
pOperationName opts info = case info of
    OperationName name -> return (name)
    _ -> return failMessage

pOperationDocumentation :: OperationPrinter
pOperationDocumentation opts info = case info of
    OperationDocumentation ref -> printFromReference opts ref
    _ -> return failMessage

pOperationSourceCode :: OperationPrinter
pOperationSourceCode opts info = case info of
    OperationSourceCode ref -> printFromReference opts ref
    _ -> return failMessage

pOperationSignature :: OperationPrinter
pOperationSignature opts info = case info of
    OperationSignature s -> return (s)
    _ -> return failMessage

pOperationInfix :: OperationPrinter
pOperationInfix opts info = case info of
    OperationInfix inf -> return (show inf)
    _ -> return failMessage

pOperationPrecedence :: OperationPrinter
pOperationPrecedence opts info = case info of
    OperationPrecedence p -> return (show p)
    _ -> return failMessage

pOperationDeterministic :: OperationPrinter
pOperationDeterministic opts info = case info of
    OperationDeterministic det -> return (show det)
    _ -> return failMessage

pOperationDemandness :: OperationPrinter
pOperationDemandness opts info = case info of
    OperationDemandness dem -> return (show dem)
    _ -> return failMessage

pOperationIndeterministic :: OperationPrinter
pOperationIndeterministic opts info = case info of
    OperationIndeterministic ind -> return (show ind)
    _ -> return failMessage

pOperationSolutionCompleteness :: OperationPrinter
pOperationSolutionCompleteness opts info = case info of
    OperationSolutionCompleteness sol -> return (show sol)
    _ -> return failMessage

pOperationTermination :: OperationPrinter
pOperationTermination opts info = case info of
    OperationTermination t -> return (show t)
    _ -> return failMessage

pOperationTotallyDefined :: OperationPrinter
pOperationTotallyDefined opts info = case info of
    OperationTotallyDefined t -> return (show t)
    _ -> return failMessage
