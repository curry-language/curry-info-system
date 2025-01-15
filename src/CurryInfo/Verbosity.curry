-----------------------------------------------------------------------------------------
--- This modules defines operations to print messages to stderr when certain verbosity options are used.
-----------------------------------------------------------------------------------------

module CurryInfo.Verbosity
  ( printStatusMessage, printDetailMessage
  , printDebugMessage, printDebugString, printErrorMessage
  ) where

import Control.Monad   ( when )

import System.IO       ( stderr, hPutStr, hPutStrLn )

import CurryInfo.Types ( Options(..) )

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

--- This action prints a message line to stderr if the Boolean function returns
--- True on the options.
printMessage :: (Options -> Bool) -> Options -> String -> IO ()
printMessage checker opts msg = when (checker opts) (printErrorMessage msg)

--- This action prints a string to stderr if the Boolean function returns
--- True on the options.
printString :: (Options -> Bool) -> Options -> String -> IO ()
printString checker opts msg = when (checker opts) (printErrorString msg)

--- This action prints a Status message if the verbosity level is high enough.
printStatusMessage :: Options -> String -> IO ()
printStatusMessage = printMessage isStatus

--- This action prints a Detail message if the verbosity level is high enough.
printDetailMessage :: Options -> String -> IO ()
printDetailMessage = printMessage isDetail

--- Prints a Debug message line if the verbosity level is high enough.
printDebugMessage :: Options -> String -> IO ()
printDebugMessage = printMessage isDebug

--- Prints a Debug message string if the verbosity level is high enough.
printDebugString :: Options -> String -> IO ()
printDebugString = printString isDebug

--- Print the argument string as a line to stderr.
printErrorMessage :: String -> IO ()
printErrorMessage msg = hPutStrLn stderr msg

--- Print the argument string to stderr.
printErrorString :: String -> IO ()
printErrorString msg = hPutStr stderr msg
