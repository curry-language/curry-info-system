module CurryAnalysisInfrastructure.Verbosity where

import CurryAnalysisInfrastructure.Options

import Control.Monad (when)

-- This action prints a line if the verbosity level in the options is at 4.
printLine :: Options -> IO ()
printLine opts = when (fullVerbosity opts) (putStrLn (replicate 20 '-'))

-- This action prints the given message if the verbosity level in the options is at 4.
printDebugMessage :: Options -> String -> IO ()
printDebugMessage opts msg = when (fullVerbosity opts) (putStrLn msg)