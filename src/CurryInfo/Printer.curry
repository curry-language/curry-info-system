module CurryInfo.Printer where

import CurryInfo.Types
import CurryInfo.Verbosity (printDebugMessage)

import System.Directory (doesFileExist)

import JSON.Data

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

pPackageName :: Printer String
pPackageName opts s = return s

pPackageVersions :: Printer [String]
pPackageVersions opts vsns = return (show vsns)

-- VERSION

pVersionVersion :: Printer String
pVersionVersion opts vsn = return (vsn)

pVersionDocumentation :: Printer String
pVersionDocumentation opts path = do
    b <- doesFileExist path
    case b of
        False -> do
            printDebugMessage opts $ "File '" ++ path ++ "' does not exist."
            return "FAILED DUE TO FILE NOT EXISTING"
        True -> do
            printDebugMessage opts $ "Reading from file '" ++ path ++ "'..."
            content <- readFile path
            return content

pVersionCategories :: Printer [String]
pVersionCategories opts cats = return (show cats)

pVersionModules :: Printer [String]
pVersionModules opts mods = return (show mods)

pVersionDependencies :: Printer [Dependency]
pVersionDependencies opts deps = return (show deps)
    
-- MODULE

pModuleName :: Printer String
pModuleName opts name = return (name)

pModuleDocumentation :: Printer Reference
pModuleDocumentation opts ref = printFromReference opts ref

pModuleSourceCode :: Printer Reference
pModuleSourceCode opts ref = printFromReference opts ref

pModuleSafe :: Printer Safe
pModuleSafe opts safe = return (show safe)

pModuleTypeclasses :: Printer [String]
pModuleTypeclasses opts cs = return (show cs)

pModuleTypes :: Printer [String]
pModuleTypes opts ts = return (show ts)

pModuleOperations :: Printer [String]
pModuleOperations opts os = return (show os)

-- TYPE

pTypeName :: Printer String
pTypeName opts name = return (name)

pTypeDocumentation :: Printer Reference
pTypeDocumentation opts ref = printFromReference opts ref

pTypeConstructors :: Printer [String]
pTypeConstructors opts cons = return (show cons)

pTypeDefinition :: Printer Reference
pTypeDefinition opts ref = printFromReference opts ref

-- TYPECLASS

pTypeclassName :: Printer String
pTypeclassName opts name = return (name)

pTypeclassDocumentation :: Printer Reference
pTypeclassDocumentation opts ref = printFromReference opts ref

pTypeclassMethods :: Printer [String]
pTypeclassMethods opts ms = return (show ms)

pTypeclassDefinition :: Printer Reference
pTypeclassDefinition opts ref = printFromReference opts ref

-- OPERATION

pOperationName :: Printer String
pOperationName opts name = return (name)

pOperationDocumentation :: Printer Reference
pOperationDocumentation opts ref = printFromReference opts ref

pOperationSourceCode :: Printer Reference
pOperationSourceCode opts ref = printFromReference opts ref

pOperationSignature :: Printer Signature
pOperationSignature opts s = return (s)

pOperationInfix :: Printer (Maybe Infix)
pOperationInfix opts inf = return (show inf)

pOperationPrecedence :: Printer (Maybe Precedence)
pOperationPrecedence opts p = return (show p)

pOperationDeterministic :: Printer Deterministic
pOperationDeterministic opts det = return (show det)

pOperationDemandness :: Printer Demandness
pOperationDemandness opts dem = return (show dem)

pOperationIndeterministic :: Printer Indeterministic
pOperationIndeterministic opts ind = return (show ind)

pOperationSolutionCompleteness :: Printer SolutionCompleteness
pOperationSolutionCompleteness opts sol = return (show sol)

pOperationTermination :: Printer Termination
pOperationTermination opts t = return (show t)

pOperationTotallyDefined :: Printer TotallyDefined
pOperationTotallyDefined opts t = return (show t)
