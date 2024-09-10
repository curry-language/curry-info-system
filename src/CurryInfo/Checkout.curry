module CurryInfo.Checkout where

import CurryInfo.Types
import CurryInfo.Paths (root)
import CurryInfo.Commands (runCmd, cmdCheckout)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IOExts (evalCmd)
import System.FilePath ((</>))

-- This functions generates the directory name for a given package and version. It is used for
-- checkout.
toCheckout :: Package -> Version -> String
toCheckout pkg vsn = pkg ++ "-" ++ vsn

-- This actions returns the path to the directory used for checkouts.
checkouts :: IO String
checkouts = do
    path <- root
    return (path </> "checkouts")

-- This action returns the path to the directory, in which the checkout of the given version
-- of the given package is stored or will be stored.
getCheckoutPath :: Package -> Version -> IO String
getCheckoutPath pkg vsn = do
    initializeCheckouts
    path <- checkouts
    return (path </> toCheckout pkg vsn)

-- This action creates a checkouts directory if it is missing.
initializeCheckouts :: IO ()
initializeCheckouts = do
    path <- checkouts
    createDirectoryIfMissing True path

{-
cmdNotFound :: Int
cmdNotFound = 127

cmdNotExecutable :: Int
cmdNotExecutable = 126

cmdSuccess :: Int
cmdSuccess = 0
-}

-- This action creates a checkout for the given version of the given package.
checkoutIfMissing :: Options -> Package -> Version -> IO (Maybe String)
checkoutIfMissing opts pkg vsn = do
    printDetailMessage opts $ "Computing checkout path for package '" ++ pkg ++ "' with version '" ++ vsn ++ "'..."
    path <- getCheckoutPath pkg vsn
    printDetailMessage opts $ "Checkout path: " ++ path
    printDetailMessage opts "Determining if checkout is necessary..."
    b <- doesDirectoryExist path
    case b of
        True -> do
            printDetailMessage opts "Directory already exists. Checkout unnecessary."
            return $ Just path
        False -> do
            printDetailMessage opts "Directory does not exist. Checkout necessary."
            printDetailMessage opts $ "Creating checkout..."
            runCmd opts $ cmdCheckout path pkg vsn
            printDetailMessage opts "Checkout created."
            return $ Just path