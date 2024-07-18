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