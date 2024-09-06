module CurryInfo.Verbosity where

import CurryInfo.Types

import System.IO (stderr, hPutStrLn)

import Control.Monad (when)

-- SUPPOSED TO BE REPLACED
-- Returns true, if the verbosity option has the highest possible value.
fullVerbosity :: Options -> Bool
fullVerbosity opts = optVerb opts >= 3

-- This action prints a line if the verbosity level in the options is at 4.
printLine :: Options -> IO ()
printLine opts = when (fullVerbosity opts) (hPutStrLn stderr (replicate 20 '-'))

-- This action prints the given message if the verbosity level in the options is at 4.
printDebugMessage :: Options -> String -> IO ()
printDebugMessage opts msg = when (fullVerbosity opts) (hPutStrLn stderr msg)