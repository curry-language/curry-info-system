module CurryInfo.Verbosity where

import CurryInfo.Types

import System.IO (stderr, hPutStrLn)

import Control.Monad (when)

isStatus :: Options -> Bool
isStatus opts = optVerb opts >= 1

isDetail :: Options -> Bool
isDetail opts = optVerb opts >= 2

isDebug :: Options -> Bool
isDebug opts = optVerb opts >= 3

printMessage :: (Options -> Bool) -> Options -> String -> IO ()
printMessage checker opts msg = when (checker opts) (hPutStrLn stderr msg)

printStatusMessage :: Options -> String -> IO ()
printStatusMessage = printMessage isStatus

printDetailMessage :: Options -> String -> IO ()
printDetailMessage = printMessage isDetail

-- This action prints the given message if the verbosity level in the options is at 4.
printDebugMessage :: Options -> String -> IO ()
printDebugMessage = printMessage isDebug
