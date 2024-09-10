module CurryInfo.Commands where

import CurryInfo.Types
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)

import System.IOExts (evalCmd)
import System.Directory (setCurrentDirectory, getCurrentDirectory)

-- This action runs the given command call and returns the result.
-- Additionaly it also prints messages to the output depending on the exit code of the command.
runCmd :: Options -> (String, IO (Int, String, String)) -> IO (Int, String, String)
runCmd opts (cmd, action) = do
    printDetailMessage opts $ "Running command '" ++ cmd ++ "'..."
    (exitCode, output, err) <- action
    case exitCode of
                127 -> do
                    printDetailMessage opts "Command could not be found."
                126 -> do
                    printDetailMessage opts "Command was not an executable."
                0 -> do
                    printDetailMessage opts "Command finished successfully."
                _ -> do
                    printDetailMessage opts $ "Command failed with exit code '" ++ show exitCode ++ "'."

    printDebugMessage opts "Command finished with output:"
    printDebugMessage opts output

    printDebugMessage opts "Command finished with error output:"
    printDebugMessage opts err

    return (exitCode, output, err)

-- This action calls CYPM to checkout the given package with the given version to the given path.
cmdCheckout :: String -> Package -> Version -> (String, IO (Int, String, String))
cmdCheckout path pkg vsn =
    let
        x@(cmd:args) = ["cypm", "checkout", "-o", path, pkg, vsn]
        action = evalCmd cmd args ""
    in (unwords x, action)

-- This action calls CYPM to install dependencies in the given path.
cmdCYPMInstall :: String -> (String, IO (Int, String, String))
cmdCYPMInstall path = 
    let
        x@(cmd:args) = ["cypm", "exec", "cypm", "install"]
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd cmd args ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (unwords x, action)

-- This action calls CYPM to update the package index in the given path.
cmdCYPMUpdate :: String -> (String, IO (Int, String, String))
cmdCYPMUpdate path =
    let
        x@(cmd:args) = ["cypm", "update"]
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd cmd args ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (unwords x, action)

-- This action calls curry to load a specific module and immediatly quits again.
-- This is done to initiate the compiler to generate files for the module (i.e. icurry).
cmdCurryLoad :: String -> Module -> (String, IO (Int, String, String))
cmdCurryLoad path m =
    let
        x@(cmd:args) = ["cypm", "exec", "curry", ":l", m, ":q"]
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            getCurrentDirectory >>= print
            (exitCode, output, err) <- evalCmd cmd args ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (unwords x, action)

-- This action calls CASS to compute the given analysis for the given module in the given path.
cmdCASS :: String -> String -> Module -> (String, IO (Int, String, String))
cmdCASS path analysis m =
    let
        x@(cmd:args) = ["cypm", "exec", "cass", "-f", "xml", analysis, m]
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd cmd args ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (unwords x, action)
