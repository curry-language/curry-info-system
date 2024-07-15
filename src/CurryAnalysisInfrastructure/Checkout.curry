module CurryAnalysisInfrastructure.Checkout where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Paths (root)

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
checkoutIfMissing :: Package -> Version -> IO (Maybe String)
checkoutIfMissing pkg vsn = do
    path <- getCheckoutPath pkg vsn
    b1 <- doesDirectoryExist path
    case b1 of
        True -> return $ Just path
        False -> do
            --"cypm checkout -o DIR PACKAGE VERSION"
            let cmd = "cypm checkout -o " ++ path ++ " " ++ pkg ++ " " ++ vsn
            (exitCode, _, _) <- evalCmd "cypm" ["checkout", "-o", path, pkg, vsn] ""
            case exitCode of
                127 -> do
                    print "Command 'cypm' was not found"
                    return Nothing
                126 -> do
                    print "Command 'cypm' is not executable"
                    return Nothing
                0 -> do
                    b2 <- doesDirectoryExist path
                    case b2 of
                        True -> return $ Just path
                        False -> do
                            print $ "Checkout for " ++ toCheckout pkg vsn ++ " failed"
                            return Nothing
                _ -> do
                    print $ "Checkout for " ++ toCheckout pkg vsn ++ " failed"
                    return Nothing  