module CurryAnalysisInfrastructure.Interface where

import CurryAnalysisInfrastructure.Paths (moduleToPath)
import CurryAnalysisInfrastructure.Checkout (getCheckoutPath, checkoutIfMissing)
import CurryAnalysisInfrastructure.Types

import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.IOExts (evalCmd)
import System.FrontendExec (FrontendTarget (..), callFrontend)

import CurryInterface.Types
import CurryInterface.Files (readCurryInterfaceFile)

icurryPath :: Package -> Version -> Module -> IO (String, String)
icurryPath pkg vsn m = do
    print "checkoutIfMissing"
    checkoutIfMissing pkg vsn
    print "checkoutPath"
    dir <- getCheckoutPath pkg vsn
    let pakcs = "pakcs-3.7.0"
    return (dir, dir ++ "/src/.curry/" ++ pakcs ++ "/" ++ moduleToPath m ++ ".icurry")

readInterface :: Package -> Version -> Module -> IO Interface
readInterface pkg vsn m = do
    print "current"
    current <- getCurrentDirectory
    print "icurrypath"
    (srcdir, path) <- icurryPath pkg vsn m
    print "file exists?"
    b <- doesFileExist path
    case b of
        True -> do
            print "yes"
            readCurryInterfaceFile path
            --content <- readFile path
            --return (Just content)
        False -> do
            print "no"
            setCurrentDirectory srcdir
            print "update"
            let updateCmd = "cypm update"
            evalCmd "cypm" ["update"] "" >>= output updateCmd
            print "install"
            let installCmd = "cypm install"
            evalCmd "cypm" ["install"] "" >>= output installCmd
            print "curry"
            let curryCmd = "curry :l " ++ m
            evalCmd "curry" [":l", m] (":q\n") >>= output curryCmd
            setCurrentDirectory current
            print "done"
            readCurryInterfaceFile path
            --readCurryInterface m
    where
    output :: String -> (Int, String, String) -> IO ()
    output cmd (exitCode, out, err) = do
        putStrLn $ "Exitcode for '" ++ cmd ++ "': " ++ show exitCode
        putStrLn $ "Output for '" ++ cmd ++ "': " ++ show out
        putStrLn $ "Error for '" ++ cmd ++ "': " ++ show err
