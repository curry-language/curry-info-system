module CurryAnalysisInfrastructure.Commands where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Options

import System.IOExts (evalCmd)
import System.Directory (setCurrentDirectory, getCurrentDirectory)

import Control.Monad (when)

runCmd :: Options -> (String, IO (Int, String, String)) -> IO (Int, String, String)
runCmd opts (cmd, action) = do
    when (fullVerbosity opts) (putStrLn $ "Running command '" ++ cmd ++ "'...")
    (exitCode, output, err) <- action
    case exitCode of
                127 -> do
                    when (fullVerbosity opts) (putStrLn $ "Command '" ++ cmd ++ "' could not be found.")
                126 -> do
                    when (fullVerbosity opts) (putStrLn $ "Command '" ++ cmd ++ "' was not an executable.")
                0 -> do
                    when (fullVerbosity opts) (putStrLn $ "Command '" ++ cmd ++ "' finished successfully.")
                _ -> do
                    when (fullVerbosity opts) (putStrLn $ "Command '" ++ cmd ++ "' failed with exit code " ++ show exitCode ++ ".")
    when (fullVerbosity opts) (putStrLn ("Command '" ++ cmd ++ "' finished with output:") >> putStrLn output)
    when (fullVerbosity opts) (putStrLn ("Command '" ++ cmd ++ "' finished with error output:") >> putStrLn err)
    return (exitCode, output, err)

cmdCheckout :: String -> Package -> Version -> (String, IO (Int, String, String))
cmdCheckout path pkg vsn =
    let
        cmd = "cypm checkout -o " ++ path ++ " " ++ pkg ++ " " ++ vsn
        action = evalCmd "cypm" ["checkout", "-o", path, pkg, vsn] ""
    in (cmd, action)
{-
cmdCheckout path pkg vsn = do
    let cmd = "cypm checkout -o " ++ path ++ " " ++ pkg ++ " " ++ vsn
    (exitCode, output, err) <- evalCmd "cypm" ["checkout", "-o", path, pkg, vsn] ""
    return (cmd, exitCode, output, err)
-}

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
{-
cmdCYPMInstall path = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    let cmd = "cypm install"
    (exitCode, output, err) <- evalCmd "cypm" ["install"] ""
    --let cmd = "cypm exec cypm install"
    --(exitCode, output, err) <- evalCmd "cypm" ["exec", "cypm", "install"] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)
-}

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
{-
cmdCYPMUpdate path = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    let cmd = "cypm update"
    (exitCode, output, err) <- evalCmd "cypm" ["update"] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)
-}

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
{-
cmdCurryLoad path m = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    getCurrentDirectory >>= print
    let cmd = "cypm exec curry :l " ++ m
    (exitCode, output, err) <- evalCmd "cypm" ["exec", "curry", ":l", m, ":q"] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)
-}

cmdCASS :: String -> String -> Module -> (String, IO (Int, String, String))
cmdCASS path analysis m =
    let
        cmd = "cass -f json " ++ analysis ++ " " ++ m
        action = do
            current <- getCurrentDirectory
            setCurrentDirectory path
            let cmd = "cass -f json " ++ analysis ++ " " ++ m
            (exitCode, output, err) <- evalCmd "cypm" ["exec", "cass", "-f", "json", analysis, m] ""
            setCurrentDirectory current
            return (exitCode, output, err)
    in (cmd, action)
{-
cmdCASS path analysis m = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    let cmd = "cass -f json " ++ analysis ++ " " ++ m
    (exitCode, output, err) <- evalCmd "cypm" ["exec", "cass", "-f", "json", analysis, m] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)
-}