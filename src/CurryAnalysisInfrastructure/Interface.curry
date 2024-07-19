module CurryAnalysisInfrastructure.Interface where

import CurryAnalysisInfrastructure.Paths (moduleToPath)
import CurryAnalysisInfrastructure.Checkout (getCheckoutPath, checkoutIfMissing)
import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Commands (runCmd, cmdCYPMInstall, cmdCurryLoad)
import CurryAnalysisInfrastructure.Options

import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.IOExts (evalCmd)
import System.FrontendExec (FrontendTarget (..), callFrontend)

import CurryInterface.Types
import CurryInterface.Files (readCurryInterfaceFile)

import Data.List (find)

icurryPath :: Options -> Package -> Version -> Module -> IO (Maybe String)
icurryPath opts pkg vsn m = do
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            let pakcs = "pakcs-3.7.1"
            return $ Just (path ++ "/src/.curry/" ++ pakcs ++ "/" ++ moduleToPath m ++ ".icurry")
        Nothing -> do
            print "icurryPath: checkout failed"
            return Nothing

readInterface :: Options -> Package -> Version -> Module -> IO (Maybe Interface)
readInterface opts pkg vsn m = do
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            print $ "Path for checkout: " ++ path
            micurry <- icurryPath opts pkg vsn m
            case micurry of
                Nothing -> do
                    print "readInterface: icurryPath failed"
                    return Nothing
                Just icurry -> do
                    print $ "Path for .icurry: " ++ icurry
                    b <- doesFileExist icurry
                    case b of
                        True -> do
                            print "File exists"
                            print "Read .icurry"
                            result <- readCurryInterfaceFile icurry
                            return $ Just result
                        False -> do
                            print "File does not exist"
                            print "Generating file"
                            runCmd opts (cmdCYPMInstall path)
                            runCmd opts (cmdCurryLoad path m)
                            print "Read .icurry"
                            result <- readCurryInterfaceFile icurry
                            return $ Just result
        Nothing -> do
            print "readInterface: checkout failed"
            return Nothing

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
