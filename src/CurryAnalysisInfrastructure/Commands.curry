module CurryAnalysisInfrastructure.Commands where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Options
import CurryAnalysisInfrastructure.Verbosity (printLine, printDebugMessage)

import System.IOExts (evalCmd)
import System.Directory (setCurrentDirectory, getCurrentDirectory)

-- This action runs the given command call and returns the result.
-- Additionaly it also prints messages to the output depending on the exit code of the command.
runCmd :: Options -> (String, IO (Int, String, String)) -> IO (Int, String, String)
runCmd opts (cmd, action) = do
    printLine opts
    printDebugMessage opts $ "Running command '" ++ cmd ++ "'..."
    (exitCode, output, err) <- action
    case exitCode of
                127 -> do
                    printDebugMessage opts "Command could not be found."
                126 -> do
                    printDebugMessage opts "Command was not an executable."
                0 -> do
                    printDebugMessage opts "Command finished successfully."
                _ -> do
                    printDebugMessage opts $ "Command failed with exit code '" ++ show exitCode ++ "'."

    printDebugMessage opts "Command finished with output:"
    printLine opts
    printDebugMessage opts output
    printLine opts

    printDebugMessage opts "Command finished with error output:"
    printLine opts
    printDebugMessage opts err
    printLine opts

    return (exitCode, output, err)

-- This action calls CYPM to checkout the given package with the given version to the given path.
cmdCheckout :: String -> Package -> Version -> (String, IO (Int, String, String))
cmdCheckout path pkg vsn =
    let
        cmd = "cypm checkout -o " ++ path ++ " " ++ pkg ++ " " ++ vsn
        action = evalCmd "cypm" ["checkout", "-o", path, pkg, vsn] ""
    in (cmd, action)

-- This action calls CYPM to install dependencies in the given path.
cmdCYPMInstall :: String -> (String, IO (Int, String, String))
cmdCYPMInstall path = 
    let
        cmd = "cpym exec cypm install"
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd "cypm" ["install"] ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (cmd, action)

-- This action calls CYPM to update the package index in the given path.
cmdCYPMUpdate :: String -> (String, IO (Int, String, String))
cmdCYPMUpdate path =
    let
        cmd = "cypm update"
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd "cypm" ["update"] ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (cmd, action)

-- This action calls curry to load a specific module and immediatly quits again.
-- This is done to initiate the compiler to generate files for the module (i.e. icurry).
cmdCurryLoad :: String -> Module -> (String, IO (Int, String, String))
cmdCurryLoad path m =
    let
        cmd = "cypm exec curry :l " ++ m ++ " :q"
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            getCurrentDirectory >>= print
            (exitCode, output, err) <- evalCmd "cypm" ["exec", "curry", ":l", m, ":q"] ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (cmd, action)

-- This action calls CASS to compute the given analysis for the given module in the given path.
cmdCASS :: String -> String -> Module -> (String, IO (Int, String, String))
cmdCASS path analysis m =
    let
        cmd = "cypm exec cass -f json " ++ analysis ++ " " ++ m
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd "cypm" ["exec", "cass", "-f", "json", analysis, m] ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (cmd, action)
