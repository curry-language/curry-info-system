module CurryAnalysisInfrastructure.Verbosity where

import CurryAnalysisInfrastructure.Options

import Control.Monad (when)

printLine :: Options -> IO ()
printLine opts = when (fullVerbosity opts) (putStrLn (replicate 20 '-'))

printDebugMessage :: Options -> String -> IO ()
printDebugMessage opts msg = when (fullVerbosity opts) (putStrLn msg)