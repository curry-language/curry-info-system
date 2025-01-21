------------------------------------------------------------------------------
--- This modules defines operations to invoke commands on the command-line.
------------------------------------------------------------------------------

module CurryInfo.Commands where

import Control.Monad    ( when )
import System.IOExts    ( evalCmd )
import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import System.FilePath  ( splitSearchPath )
import System.Path      ( fileInPath )

import CurryInfo.Helper    ( quote )
import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage, printErrorMessage )

-- This action runs the given command call and returns the result.
-- Additionaly it also prints messages to the output depending on the exit code
-- of the command.
runCmd :: Options -> (String, IO (Int, String, String))
       -> IO (Int, String, String)
runCmd opts (cmd, action) = do
  printDetailMessage opts $ "Running command " ++ quote cmd ++ "..."
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
  in (unwords x, evalCmd cmd args "")

-- This action calls CPM to install dependencies in the given path.
cmdCPMInstall :: Options -> String -> (String, IO (Int, String, String))
cmdCPMInstall opts path = 
  let x@(cmd:args) = ["cypm", "install", "--noexec"]
  in (unwords x, evalCmdInDirectory opts path cmd args "")

--- Generate command to compute the load path of the package stored
---in the given path.
cmdCPMPath :: Options -> String -> (String, IO (Int, String, String))
cmdCPMPath opts path = 
  let x@(cmd:args) = ["cypm", "deps", "--path"]
  in (unwords x, evalCmdInDirectory opts path cmd args "")

-- This action calls CPM to update the package index in the given path.
cmdCPMUpdate :: Options -> String -> (String, IO (Int, String, String))
cmdCPMUpdate opts path =
  let x@(cmd:args) = ["cypm", "update"]
  in (unwords x, evalCmdInDirectory opts path cmd args "")

-- This action calls curry to load a specific module and immediatly quits again.
-- This is done to initiate the compiler to generate files for the module
-- (i.e., interface `.icurry` files).
cmdCurryLoad :: Options -> String -> Module -> (String, IO (Int, String, String))
cmdCurryLoad opts path m =
  let x@(cmd:args) = ["cypm", "curry", ":l", m, ":q"]
  in (unwords x, evalCmdInDirectory opts path cmd args "")

-- This action calls CASS to compute the given analysis for the given module
-- in the given path.
cmdCASS :: Options -> FilePath -> String -> Module
        -> (String, IO (Int, String, String))
cmdCASS opts path analysis m =
  let x@(cmd:args) = [ "cypm", "exec", "cass", "-q", "-f", "JSONTerm"
                     , analysis, m]
      action = do getPackageLoadPath opts path -- maybe install package...
                  evalCmdInDirectory opts path cmd args ""
  in (unwords x, action)

-- This action calls `curry-calltypes` to compute results for the `failfree`
-- analysis on the given module in the given path.
cmdCallTypes :: Options -> [String] -> FilePath -> String -> Module
             -> (String, IO (Int, String, String))
cmdCallTypes opts ctopts path _ m =
  let x@(cmd:args) = [ "cypm", "exec", "curry-calltypes", "--format=json"] ++
                     ctopts ++ [m]
      action = do getPackageLoadPath opts path -- maybe install package...
                  evalCmdInDirectory opts path cmd args ""
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
    else return (1, "", "Binary " ++ quote cmd ++ " not found in PATH!")

------------------------------------------------------------------------------
--- Gets the Curry load path of a package stored in the given path
--- by CPM.
getPackageLoadPath :: Options -> FilePath -> IO (Maybe [String])
getPackageLoadPath opts path = do
  printDetailMessage opts $ "Get load path of package " ++ quote path ++ "..."
  (ec1,out1,_) <- runCmd opts $ cmdCPMPath opts path
  if ec1 == 0
    then returnOut out1
    else do
      printDetailMessage opts $ "Getting load path failed, thus, " ++
        "installing package in " ++ quote path ++ "..."
      (ec2,out2,err2) <- runCmd opts $ cmdCPMInstall opts path
      if ec2 > 0
        then do printErrorMessage "Installing package failed!"
                printErrorMessage $ "Outputs:\n" ++ out2 ++ err2
                return Nothing
        else do
          printDetailMessage opts $
            "Get load path of package " ++ quote path ++ "..."
          (ec3,out3,err3) <- runCmd opts $ cmdCPMPath opts path
          if ec3 > 0
            then do printErrorMessage "Getting package load path failed!"
                    printErrorMessage $ "Outputs:\n" ++ out3 ++ err3
                    return Nothing
            else returnOut out3
 where
  returnOut out = do
    let ls = lines out
    return $ Just $ if null ls then [] else splitSearchPath (head ls)

------------------------------------------------------------------------------