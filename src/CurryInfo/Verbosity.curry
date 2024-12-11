-----------------------------------------------------------------------------------------
--- This modules defines operations to print messages to stderr when certain verbosity options are used.
-----------------------------------------------------------------------------------------

module CurryInfo.Verbosity where

import CurryInfo.Types

import System.IO (stderr, hPutStrLn)

import Control.Monad (when)

statusVerbosity :: Int
statusVerbosity = 1

detailVerbosity :: Int
detailVerbosity = 2

debugVerbosity :: Int
debugVerbosity = 3

--- This operation checks whether the verbosity level is high enough
--- for Status messages.
isStatus :: Options -> Bool
isStatus opts = optVerb opts >= statusVerbosity

--- This operation checks whether the verbosity level is high enough
--- for Detail messages.
isDetail :: Options -> Bool
isDetail opts = optVerb opts >= detailVerbosity

--- This operation checks whether the verbosity level is high enough
--- for Debug messages.
isDebug :: Options -> Bool
isDebug opts = optVerb opts >= debugVerbosity

--- This action prints a message to stderr if the boolean function returns
--- True on the options.
printMessage :: (Options -> Bool) -> Options -> String -> IO ()
printMessage checker opts msg = when (checker opts) (hPutStrLn stderr msg)

--- This action prints a Status message if the verbosity level is high enough.
printStatusMessage :: Options -> String -> IO ()
printStatusMessage = printMessage isStatus

--- This action prints a Detail message if the verbosity level is high enough.
printDetailMessage :: Options -> String -> IO ()
printDetailMessage = printMessage isDetail

--- This action prints a Debug message if the verbosity level is high enough.
printDebugMessage :: Options -> String -> IO ()
printDebugMessage = printMessage isDebug
