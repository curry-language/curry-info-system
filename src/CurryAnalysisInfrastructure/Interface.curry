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

icurryPath :: Package -> Version -> Module -> IO String
icurryPath pkg vsn m = do
    path <- getCheckoutPath pkg vsn
    let pakcs = "pakcs-3.7.1"
    return (path ++ "/src/.curry/" ++ pakcs ++ "/" ++ moduleToPath m ++ ".icurry")

readInterface :: Options -> Package -> Version -> Module -> IO Interface
readInterface opts pkg vsn m = do
    path <- getCheckoutPath pkg vsn
    print $ "Path for checkout: " ++ path
    icurry <- icurryPath pkg vsn m
    print $ "Path for .icurry: " ++ icurry
    b <- doesFileExist icurry
    case b of
        True -> do
            print "File exists"
            print "Read .icurry"
            readCurryInterfaceFile icurry
        False -> do
            print "File does not exist"
            print "Generating file"
            runCmd opts (cmdCYPMInstall path)
            runCmd opts (cmdCurryLoad path m)
            print "Read .icurry"
            readCurryInterfaceFile icurry

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
