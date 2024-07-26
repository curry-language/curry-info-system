module CurryAnalysisInfrastructure.Interface where

import CurryAnalysisInfrastructure.Checkout (getCheckoutPath, checkoutIfMissing)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Commands (runCmd, cmdCYPMInstall, cmdCurryLoad)
import CurryAnalysisInfrastructure.Options (Options, fullVerbosity)

import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.IOExts (evalCmd)
import System.FrontendExec (FrontendTarget (..), callFrontend)
import System.CurryPath (currySubdir, modNameToPath)

import CurryInterface.Types
import CurryInterface.Files (readCurryInterfaceFile)
import CurryInterface.Pretty (defaultOptions, ppConstructor, ppNewConstructor, ppType, ppMethodDecl, ppQualType)

import Data.List (find)

import Control.Monad (when)

import Text.Pretty (pPrint)

icurryPath :: Package -> Version -> Module -> IO String
icurryPath pkg vsn m = do
    path <- getCheckoutPath pkg vsn
    return (path ++ "/src/" ++ currySubdir ++ "/" ++ modNameToPath m ++ ".icurry")

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

getDeclarations :: Interface -> [IDecl]
getDeclarations (Interface _ _ decls) = decls

-- TYPE

getAllTypes :: [IDecl] -> [IDecl]
getAllTypes = filter isType

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

getHiddenTypes :: [IDecl] -> [IDecl]
getHiddenTypes = filter isHiddenType

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

getAllClasses :: [IDecl] -> [IDecl]
getAllClasses = filter isClass

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

getHiddenClasses :: [IDecl] -> [IDecl]
getHiddenClasses = filter isHiddenClass

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

getOperations :: [IDecl] -> [IDecl]
getOperations = filter isOperation

getOperationName :: IDecl -> Maybe String
getOperationName decl = case decl of
    IFunctionDecl name _ _ _ -> Just $ idName $ qidIdent name
    _ -> Nothing

getOperationDecl :: String -> [IDecl] -> Maybe IDecl
getOperationDecl o = find (\decl -> Just o == getOperationName decl)

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

getOperationSignature :: IDecl -> Maybe Signature
getOperationSignature decl = case decl of
    IFunctionDecl _ _ _ t -> Just $ (pPrint . ppQualType defaultOptions) t
    _ -> Nothing

getInfixName :: IDecl -> Maybe String
getInfixName decl = case decl of
    IInfixDecl _ _ name -> Just $ idName $ qidIdent name
    _ -> Nothing

getInfixDecl :: Operation -> [IDecl] -> Maybe IDecl
getInfixDecl o = find (\decl -> Just o == getInfixName decl)

getOperationInfix :: IDecl -> Maybe CurryAnalysisInfrastructure.Types.Infix
getOperationInfix decl = case decl of
    IInfixDecl i _ _ -> case i of
        CurryInterface.Types.Infix -> Just CurryAnalysisInfrastructure.Types.Infix
        CurryInterface.Types.InfixL -> Just CurryAnalysisInfrastructure.Types.InfixL
        CurryInterface.Types.InfixR -> Just CurryAnalysisInfrastructure.Types.InfixR
    _ -> Nothing

getOperationPrecedence :: IDecl -> Maybe CurryAnalysisInfrastructure.Types.Precedence
getOperationPrecedence decl = case decl of
    IInfixDecl _ p _ -> Just p
    _ -> Nothing