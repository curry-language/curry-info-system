module CurryAnalysisInfrastructure.Checkout where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths (root)
import CurryAnalysisInfrastructure.Commands (runCmd, cmdCheckout)
import CurryAnalysisInfrastructure.Options

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IOExts (evalCmd)

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
    print "Computing path for checkout"
    path <- getCheckoutPath pkg vsn
    print $ "Path for checkout: " ++ path
    b1 <- doesDirectoryExist path
    case b1 of
        True -> do
            print "Checkout unnecessary"
            return $ Just path
        False -> do
            print "Checkout necessary"
            (exitCode, output, err) <- runCmd opts $ cmdCheckout path pkg vsn
            case exitCode of
                127 -> do
                    print "Command 'cypm' was not found"
                    return Nothing
                126 -> do
                    print "Command 'cypm' is not executable"
                    return Nothing
                0 -> do
                    print "Checkout successfull"
                    return $ Just path
                _ -> do
                    print $ "Checkout for " ++ toCheckout pkg vsn ++ " failed"
                    return Nothing  