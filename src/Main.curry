module Main where

import CurryInfo.Types
import CurryInfo.Options (processOptions, getObject)
import CurryInfo.Information (getInfos, printResult)

import Data.Maybe (isJust)

import System.Environment (getArgs)
import System.Process (exitWith)

import Control.Monad (unless)


main :: IO ()
main = do
        args <- getArgs
        (opts, args2) <- processOptions "" args
        let obj = getObject opts
        unless (isJust (optPackage opts)) (putStrLn "Package name is required" >> exitWith 1)
        res <- getInfos opts obj args2
        printResult res
        return ()
