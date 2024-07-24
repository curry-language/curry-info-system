module CurryAnalysisInfrastructure.Interface where

import CurryAnalysisInfrastructure.Paths (moduleToPath)
import CurryAnalysisInfrastructure.Checkout (getCheckoutPath, checkoutIfMissing)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Commands (runCmd, cmdCYPMInstall, cmdCurryLoad)
import CurryAnalysisInfrastructure.Options (Options, fullVerbosity)

import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.IOExts (evalCmd)
import System.FrontendExec (FrontendTarget (..), callFrontend)

import CurryInterface.Types
import CurryInterface.Files (readCurryInterfaceFile)
import CurryInterface.Pretty (defaultOptions, ppConstructor, ppNewConstructor, ppType, ppMethodDecl)

import Data.List (find)

import Control.Monad (when)

import Text.Pretty (pPrint)

icurryPath :: Package -> Version -> Module -> IO String
icurryPath pkg vsn m = do
    path <- getCheckoutPath pkg vsn
    let pakcs = "pakcs-3.7.1"
    return (path ++ "/src/.curry/" ++ pakcs ++ "/" ++ moduleToPath m ++ ".icurry")

readInterface :: Options -> Package -> Version -> Module -> IO (Maybe Interface)
readInterface opts pkg vsn m = do
    when (fullVerbosity opts) (putStrLn $ "Checkout for package " ++ pkg ++ " with version " ++ vsn ++ " if necessary...")
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            when (fullVerbosity opts) (putStrLn $ "Computing path to icurry file...")
            icurry <- icurryPath pkg vsn m
            when (fullVerbosity opts) (putStrLn $ "Path to icurry file: " ++ icurry)
            when (fullVerbosity opts) (putStrLn $ "Checking whether icurry file exists...")
            b <- doesFileExist icurry
            case b of
                True -> do
                    when (fullVerbosity opts) (putStrLn $ "icurry file exists.")
                    when (fullVerbosity opts) (putStrLn $ "Reading interface...")
                    result <- readCurryInterfaceFile icurry
                    return $ Just result
                False -> do
                    when (fullVerbosity opts) (putStrLn $ "icurry file does not exist.")
                    when (fullVerbosity opts) (putStrLn $ "Generating icurry file...")
                    runCmd opts (cmdCYPMInstall path)
                    runCmd opts (cmdCurryLoad path m)
                    when (fullVerbosity opts) (putStrLn $ "Reading interface...")
                    result <- readCurryInterfaceFile icurry
                    return $ Just result
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Checkout failed.")
            return Nothing

-- TYPE

getAllTypes :: Interface -> [IDecl]
getAllTypes (Interface _ _ decls) = filter isType decls

isType :: IDecl -> Bool
isType decl = case decl of
    IDataDecl _ _ _ _ _ -> True
    INewtypeDecl _ _ _ _ _ -> True
    ITypeDecl _ _ _ _ -> True
    _ -> False

getTypeName :: IDecl -> Maybe String
getTypeName decl = case decl of
    IDataDecl name _ _ _ _ -> Just $ idName $ qidIdent name
    INewtypeDecl name _ _ _ _ -> Just $ idName $ qidIdent name
    ITypeDecl name _ _ _ -> Just $ idName $ qidIdent name
    _ -> Nothing

getTypeDecl :: String -> [IDecl] -> Maybe IDecl
getTypeDecl t = find (\decl -> Just t == getTypeName decl)

getHiddenTypes :: Interface -> [IDecl]
getHiddenTypes (Interface _ _ decls) = filter isHiddenType decls

getHiddenTypeName :: IDecl -> Maybe String
getHiddenTypeName decl = case decl of
    HidingDataDecl name _ _ -> Just $ idName $ qidIdent name
    _ -> Nothing

isHiddenType :: IDecl -> Bool
isHiddenType decl = case decl of
    HidingDataDecl _ _ _ -> True
    _ -> False

getTypeConstructors :: IDecl -> Maybe [Constructor]
getTypeConstructors decl = case decl of
    IDataDecl _ _ _ constructors _ -> Just $ map (pPrint . ppConstructor defaultOptions) constructors
    INewtypeDecl _ _ _ constructor _ -> Just [(pPrint . ppNewConstructor defaultOptions) constructor]
    ITypeDecl _ _ _ t -> Just [(pPrint . ppType defaultOptions 0) t]
    _ -> Nothing

-- TYPECLASS

getAllClasses :: Interface -> [IDecl]
getAllClasses (Interface _ _ decls) = filter isClass decls

isClass :: IDecl -> Bool
isClass decl = case decl of
    IClassDecl _ _ _ _ _ _ -> True
    _ -> False

getClassName :: IDecl -> Maybe String
getClassName decl = case decl of
    IClassDecl _ name _ _ _ _ -> Just $ idName $ qidIdent name
    _ -> Nothing

getClassDecl :: String -> [IDecl] -> Maybe IDecl
getClassDecl c = find (\decl -> Just c == getClassName decl)

getHiddenClasses :: Interface -> [IDecl]
getHiddenClasses (Interface _ _ decls) = filter isHiddenClass decls

isHiddenClass :: IDecl -> Bool
isHiddenClass decl = case decl of
    HidingClassDecl _ _ _ _ -> True
    _ -> False

getHiddenClassName :: IDecl -> Maybe String
getHiddenClassName decl = case decl of
    HidingClassDecl _ name _ _ -> Just $ idName $ qidIdent name
    _ -> Nothing

getClassMethods :: IDecl -> Maybe [Method]
getClassMethods decl = case decl of
    IClassDecl _ _ _ _ methods _ -> Just $ map (pPrint . ppMethodDecl defaultOptions) methods
    _ -> Nothing

-- OPERATION

isOperation :: IDecl -> Bool
isOperation decl = case decl of
    IFunctionDecl _ _ _ _ -> True
    _ -> False

getOperations :: Interface -> [IDecl]
getOperations (Interface _ _ decls) = filter isOperation decls

getOperationName :: IDecl -> Maybe String
getOperationName decl = case decl of
    IFunctionDecl name _ _ _ -> Just $ idName $ qidIdent name
    _ -> Nothing

getOperationArity :: IDecl -> Maybe Int
getOperationArity decl = case decl of
    IFunctionDecl _ _ arity _ -> Just arity
    _ -> Nothing

findOperation :: [IDecl] -> Operation -> Maybe IDecl
findOperation decls op = find (checker op) decls
    where
    checker :: Operation -> IDecl -> Bool
    checker o decl = case decl of
        IFunctionDecl name _ _ _ -> o == idName (qidIdent name)
        _ -> False
