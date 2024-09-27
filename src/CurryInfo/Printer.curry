module CurryInfo.Printer where

import CurryInfo.Types
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Helper (slice)

import System.Directory (doesFileExist)

import JSON.Data

-- PACKAGE

pPackageName :: Printer String
pPackageName _ s = return s

pPackageVersions :: Printer [String]
pPackageVersions _ vsns = return (show vsns)

-- VERSION

pVersionVersion :: Printer String
pVersionVersion _ vsn = return vsn

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
pVersionCategories _ cats = return (show cats)

pVersionModules :: Printer [String]
pVersionModules _ mods = return (show mods)

pVersionDependencies :: Printer [Dependency]
pVersionDependencies _ deps = return (show deps)
    
-- MODULE

pModuleName :: Printer String
pModuleName _ name = return name

pModuleDocumentation :: Printer Reference
pModuleDocumentation opts ref = printFromReference opts ref

pModuleSourceCode :: Printer Reference
pModuleSourceCode opts ref = printFromReference opts ref

pModuleSafe :: Printer Safe
pModuleSafe _ safe = return (show safe)

pModuleTypeclasses :: Printer [String]
pModuleTypeclasses _ cs = return (show cs)

pModuleTypes :: Printer [String]
pModuleTypes _ ts = return (show ts)

pModuleOperations :: Printer [String]
pModuleOperations _ os = return (show os)

-- TYPE

pTypeName :: Printer String
pTypeName _ name = return name

pTypeDocumentation :: Printer Reference
pTypeDocumentation opts ref = printFromReference opts ref

pTypeConstructors :: Printer [String]
pTypeConstructors _ cons = return (show cons)

pTypeDefinition :: Printer Reference
pTypeDefinition opts ref = printFromReference opts ref

-- TYPECLASS

pTypeclassName :: Printer String
pTypeclassName _ name = return name

pTypeclassDocumentation :: Printer Reference
pTypeclassDocumentation opts ref = printFromReference opts ref

pTypeclassMethods :: Printer [String]
pTypeclassMethods _ ms = return (show ms)

pTypeclassDefinition :: Printer Reference
pTypeclassDefinition opts ref = printFromReference opts ref

-- OPERATION

pOperationName :: Printer String
pOperationName _ name = return name

pOperationDocumentation :: Printer Reference
pOperationDocumentation opts ref = printFromReference opts ref

pOperationSourceCode :: Printer Reference
pOperationSourceCode opts ref = printFromReference opts ref

pOperationSignature :: Printer Signature
pOperationSignature _ s = return s

pOperationInfix :: Printer (Maybe Infix)
pOperationInfix _ inf = return (show inf)

pOperationPrecedence :: Printer (Maybe Precedence)
pOperationPrecedence _ p = return (show p)

pOperationDeterministic :: Printer Deterministic
pOperationDeterministic _ det = return (show det)

pOperationDemandness :: Printer Demandness
pOperationDemandness _ dem = return (show dem)

pOperationIndeterministic :: Printer Indeterministic
pOperationIndeterministic _ ind = return (show ind)

pOperationSolutionCompleteness :: Printer SolutionCompleteness
pOperationSolutionCompleteness _ sol = return (show sol)

pOperationTermination :: Printer Termination
pOperationTermination _ t = return (show t)

pOperationTotallyDefined :: Printer TotallyDefined
pOperationTotallyDefined _ t = return (show t)

-- HELPER

-- This action returns the content of the file the given reference points to.
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
