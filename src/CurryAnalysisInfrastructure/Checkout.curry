module CurryAnalysisInfrastructure.Checkout where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths (root)
import CurryAnalysisInfrastructure.Commands (runCmd, cmdCheckout)
import CurryAnalysisInfrastructure.Options

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IOExts (evalCmd)

import Control.Monad (when)

-- This functions generates the directory name for a given package and version. It is used for
-- checkout.
toCheckout :: Package -> Version -> String
toCheckout pkg vsn = pkg ++ "-" ++ vsn

-- This actions returns the path to the directory used for checkouts.
checkouts :: IO String
checkouts = do
    path <- root
    return (path ++ "checkouts/")

-- This action returns the path to the directory, in which the checkout of the given version
-- of the given package is stored or will be stored.
getCheckoutPath :: Package -> Version -> IO String
getCheckoutPath pkg vsn = do
    initializeCheckouts
    path <- checkouts
    return (path ++ toCheckout pkg vsn)

-- This action creates a checkouts directory if it is missing.
initializeCheckouts :: IO ()
initializeCheckouts = do
    path <- checkouts
    createDirectoryIfMissing True path

cmdNotFound :: Int
cmdNotFound = 127

cmdNotExecutable :: Int
cmdNotExecutable = 126

cmdSuccess :: Int
cmdSuccess = 0

-- This action creates a checkout for the given version of the given package.
checkoutIfMissing :: Options -> Package -> Version -> IO (Maybe String)
checkoutIfMissing opts pkg vsn = do
    when (fullVerbosity opts) (putStrLn $ "Computing checkout path for package " ++ pkg ++ " with version " ++ vsn ++ "...")
    path <- getCheckoutPath pkg vsn
    when (fullVerbosity opts) (putStrLn $ "Checkout path: " ++ path)
    when (fullVerbosity opts) (putStrLn $ "Checking whether checkout is necessary...")
    b <- doesDirectoryExist path
    case b of
        True -> do
            when (fullVerbosity opts) (putStrLn $ "Directory already exists. Checkout unnecessary.")
            return $ Just path
        False -> do
            when (fullVerbosity opts) (putStrLn $ "Directoy does not exist. Checkout necessary.")
            when (fullVerbosity opts) (putStrLn $ "Creating checkout for package " ++ pkg ++ " with version " ++ vsn ++ "...")
            runCmd opts $ cmdCheckout path pkg vsn
            return $ Just path