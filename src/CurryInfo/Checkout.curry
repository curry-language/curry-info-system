-----------------------------------------------------------------------------------------
--- This modules defines operations around checking out packages (checkout to local cache, paths of checkouts).
-----------------------------------------------------------------------------------------

module CurryInfo.Checkout where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IOExts (evalCmd)
import System.FilePath ((</>))

import CurryInfo.Commands     ( runCmd, cmdCheckout, cmdCPMInstall )
import CurryInfo.Helper       ( quote )
import CurryInfo.Paths        ( getRoot )
import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Verbosity    ( printStatusMessage, printDetailMessage
                              , printDebugMessage, printErrorMessage )

-- This functions generates the directory name for a given package and version. It is used for
-- checkout.
toCheckout :: Package -> Version -> String
toCheckout pkg vsn = pkg ++ "-" ++ vsn

-- This actions returns the path to the directory used for checkouts.
getCheckoutRoot :: IO String
getCheckoutRoot = fmap (</> "checkouts") getRoot

-- This action returns the path to the directory, in which the checkout
-- of the given version of the given package is stored or will be stored.
getCheckoutPath :: Package -> Version -> IO String
getCheckoutPath pkg vsn = do
  initializeCheckouts
  path <- getCheckoutRoot
  return (path </> toCheckout pkg vsn)

-- This action creates a checkouts directory if it is missing.
initializeCheckouts :: IO ()
initializeCheckouts = do
  path <- getCheckoutRoot
  createDirectoryIfMissing True path

{-
cmdNotFound :: Int
cmdNotFound = 127

cmdNotExecutable :: Int
cmdNotExecutable = 126

cmdSuccess :: Int
cmdSuccess = 0
-}

--- Checkout a given package with a specified version of the given package
--- and return the directory where the package is stored.
checkoutIfMissing :: Options -> Package -> Version -> IO (Maybe String)
checkoutIfMissing opts pkg vsn = do
  printDetailMessage opts $ "Computing checkout path for package '" ++ pkg ++
                            "' with version '" ++ vsn ++ "'..."
  path <- getCheckoutPath pkg vsn
  printDetailMessage opts $ "Checkout path: " ++ path
  printDetailMessage opts "Determining if checkout is necessary..."
  expath <- doesDirectoryExist path
  case expath of
    True -> do
      printDetailMessage opts "Directory already exists. Checkout unnecessary."
      return $ Just path
    False -> do
      printDetailMessage opts "Directory does not exist. Checkout necessary."
      printDetailMessage opts $ "Creating checkout..."
      runCmd opts $ cmdCheckout opts path pkg vsn
      runCmd opts $ cmdCPMInstall opts path
      b <- doesDirectoryExist path
      case b of
        True  -> do printDetailMessage opts "Checkout created."
                    return $ Just path
        False -> do printErrorMessage $ "Checkout of " ++ quote pkg ++
                      " with version " ++ quote vsn ++ " failed!"
                    return Nothing
