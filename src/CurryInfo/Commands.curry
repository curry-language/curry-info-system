------------------------------------------------------------------------------
--- This modules defines operations to invoke commands on the command-line.
------------------------------------------------------------------------------

module CurryInfo.Commands where

import Control.Monad       ( when )
import CurryInfo.Types
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage )

import System.IOExts    ( evalCmd )
import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import System.Path      ( fileInPath )

-- This action runs the given command call and returns the result.
-- Additionaly it also prints messages to the output depending on the exit code
-- of the command.
runCmd :: Options -> (String, IO (Int, String, String))
       -> IO (Int, String, String)
runCmd opts (cmd, action) = do
  printDetailMessage opts $ "Running command '" ++ cmd ++ "'..."
  (ecode, output, err) <- action
  when (ecode > 0) $ do
    printStatusMessage opts $ "COMMAND: " ++ cmd ++ "\n" ++
      case ecode of
        127 -> "Command could not be found."
        126 -> "Command was not an executable."
        _   -> "Failed with exit code " ++ show ecode ++ "."

  printDebugMessage opts "Command finished with output:"
  printDebugMessage opts output

  when (not (null err) && ecode > 0) $ do
    printStatusMessage opts "COMMAND FINISHED WITH ERROR OUTPUT:"
    printStatusMessage opts err

  return (ecode, output, err)

-- This action calls CPM to checkout the given package with the given version
-- to the given path.
cmdCheckout :: Options -> String -> Package -> Version
            -> (String, IO (Int,String,String))
cmdCheckout _ path pkg vsn =
  let x@(cmd:args) = ["cypm", "checkout", "-o", path, pkg, vsn]
      action = evalCmd cmd args ""
  in (unwords x, action)

-- This action calls CPM to install dependencies in the given path.
cmdCPMInstall :: Options -> String -> (String, IO (Int, String, String))
cmdCPMInstall opts path = 
  let x@(cmd:args) = ["cypm", "install", "--noexec"]
      action = evalCmdInDirectory opts path cmd args ""
  in (unwords x, action)

-- This action calls CPM to update the package index in the given path.
cmdCPMUpdate :: Options -> String -> (String, IO (Int, String, String))
cmdCPMUpdate opts path =
  let x@(cmd:args) = ["cypm", "update"]
      action = evalCmdInDirectory opts path cmd args ""
  in (unwords x, action)

-- This action calls curry to load a specific module and immediatly quits again.
-- This is done to initiate the compiler to generate files for the module
-- (i.e., interface `.icurry` files).
cmdCurryLoad :: Options -> String -> Module -> (String, IO (Int, String, String))
cmdCurryLoad opts path m =
  let x@(cmd:args) = ["cypm", "curry", ":l", m, ":q"]
      action = evalCmdInDirectory opts path cmd args ""
  in (unwords x, action)

-- This action calls CASS to compute the given analysis for the given module
-- in the given path.
cmdCASS :: Options -> String -> String -> Module
        -> (String, IO (Int, String, String))
cmdCASS opts path analysis m =
  let x@(cmd:args) = ["cypm", "exec", "cass", "-q", "-f", "JSONTerm"
                     , analysis, m]
      action = evalCmdInDirectory opts path cmd args ""
  in (unwords x, action)

-- This action calls `curry-calltypes` to compute results for the `failfree`
-- analysis on the given module in the given path.
cmdCallTypes :: Options -> FilePath -> String -> Module
             -> (String, IO (Int, String, String))
cmdCallTypes opts path _ m =
  let x@(cmd:args) = [ "cypm", "exec", "curry-calltypes", "-v1"
                     , "--format=json", m]
      action = evalCmdInDirectory opts path cmd args ""
  in (unwords x, action)

-- Run `evalCmd` in a given directory.
evalCmdInDirectory :: Options -> FilePath -> String -> [String] -> String
                   -> IO (Int,String,String)
evalCmdInDirectory opts path cmd args inp = do
  binexists <- fileInPath cmd
  if binexists
    then do current <- getCurrentDirectory
            printDebugMessage opts $ "Switch to directory: " ++ path
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd cmd args inp
            setCurrentDirectory current
            return (exitCode, output, err)
    else return (1, "", "Binary '" ++ cmd ++ "' not found in PATH!")
