module CurryAnalysisInfrastructure.Commands where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Options

import System.IOExts (evalCmd)
import System.Directory (setCurrentDirectory, getCurrentDirectory)

runCmd :: Options -> IO (String, Int, String, String) -> IO (Int, String, String)
runCmd opts action = do
    (cmd, exitCode, output, err) <- action
    print cmd
    print exitCode
    print output
    print err
    print ""
    return (exitCode, output, err)

cmdCheckout :: String -> Package -> Version -> IO (String, Int, String, String)
cmdCheckout path pkg vsn = do
    let cmd = "cypm checkout -o " ++ path ++ " " ++ pkg ++ " " ++ vsn
    (exitCode, output, err) <- evalCmd "cypm" ["checkout", "-o", path, pkg, vsn] ""
    return (cmd, exitCode, output, err)

cmdCYPMInstall :: String -> IO (String, Int, String, String)
cmdCYPMInstall path = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    let cmd = "cypm install"
    (exitCode, output, err) <- evalCmd "cypm" ["install"] ""
    --let cmd = "cypm exec cypm install"
    --(exitCode, output, err) <- evalCmd "cypm" ["exec", "cypm", "install"] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)

cmdCYPMUpdate :: String -> IO (String, Int, String, String)
cmdCYPMUpdate path = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    let cmd = "cypm update"
    (exitCode, output, err) <- evalCmd "cypm" ["update"] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)

cmdCurryLoad :: String -> Module -> IO (String, Int, String, String)
cmdCurryLoad path m = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    getCurrentDirectory >>= print
    let cmd = "cypm exec curry :l " ++ m
    (exitCode, output, err) <- evalCmd "cypm" ["exec", "curry", ":l", m, ":q"] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)

cmdCASS :: String -> String -> Module -> IO (String, Int, String, String)
cmdCASS path analysis m = do
    current <- getCurrentDirectory
    setCurrentDirectory path
    let cmd = "cass -f json " ++ analysis ++ " " ++ m
    (exitCode, output, err) <- evalCmd "cypm" ["exec", "cass", "-f", "json", analysis, m] ""
    setCurrentDirectory current
    return (cmd, exitCode, output, err)

testPath :: String
testPath = "/home/dennis/tmp/.curryanalysis/checkouts/json-3.0.0"

testModule :: Module
testModule = "JSON.Data"