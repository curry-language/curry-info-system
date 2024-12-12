-----------------------------------------------------------------------------------------
--- This modules defines operations to invoke commands on the command-line.
-----------------------------------------------------------------------------------------

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
  (exitCode, output, err) <- action
  case exitCode of
    127 -> printDetailMessage opts "Command could not be found."
    126 -> printDetailMessage opts "Command was not an executable."
    0   -> printDetailMessage opts "Command finished successfully."
    _   -> printDetailMessage opts $ "Command failed with exit code '" ++
                                     show exitCode ++ "'."

  printDebugMessage opts "Command finished with output:"
  printDebugMessage opts output

  when (not (null err) && exitCode > 0) $ do
    printStatusMessage opts "COMMAND FINISHED WITH ERROR OUTPUT:"
    printStatusMessage opts err

  return (exitCode, output, err)

-- This action calls CYPM to checkout the given package with the given version to the given path.
cmdCheckout :: String -> Package -> Version -> (String, IO (Int, String, String))
cmdCheckout path pkg vsn =
  let x@(cmd:args) = ["cypm", "checkout", "-o", path, pkg, vsn]
      action = evalCmd cmd args ""
  in (unwords x, action)

-- This action calls CYPM to install dependencies in the given path.
cmdCYPMInstall :: String -> (String, IO (Int, String, String))
cmdCYPMInstall path = 
  let x@(cmd:args) = ["cypm", "install"]
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
  let x@(cmd:args) = ["cypm", "update"]
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
  let x@(cmd:args) = ["cypm", "curry", ":l", m, ":q"]
      action = do
        current <- getCurrentDirectory
        setCurrentDirectory path
        --getCurrentDirectory >>= print
        (exitCode, output, err) <- evalCmd cmd args ""
        setCurrentDirectory current
        return (exitCode, output, err)
  in (unwords x, action)

-- This action calls CASS to compute the given analysis for the given module in the given path.
cmdCASS :: String -> String -> Module -> (String, IO (Int, String, String))
cmdCASS path analysis m =
  let execbin = if analysis == "FailFree" then "curry-calltypes"
                                          else "cass"
      x@(cmd:args) = ["cypm", "exec", execbin] ++
                     if analysis == "FailFree"
                       then ["--format=json", m]
                       else ["-f", "JSONTerm", analysis, m]

      action = do
        binexists <- fileInPath execbin
        if binexists
          then do
            current <- getCurrentDirectory
            setCurrentDirectory path
            (exitCode, output, err) <- evalCmd cmd args ""
            setCurrentDirectory current
            return (exitCode, output, err)
          else return (1, "", "Binary '" ++ execbin ++ "' not found in PATH!")

  in (unwords x, action)
