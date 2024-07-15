module Main where

import CurryAnalysisInfrastructure.Interface (getInfos)
import CurryAnalysisInfrastructure.Options (Options (..), processOptions)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    (opts, args2) <- processOptions "" args

    let pkg = maybe [] (\x -> [x])                  (optPackage opts)
    let vsn = maybe [] (\x -> ["versions", x])      (optVersion opts)
    let m   = maybe [] (\x -> ["modules", x])       (optModule opts)
    let t   = maybe [] (\x -> ["types", x])         (optType opts)
    let c   = maybe [] (\x -> ["typeclasses", x])   (optTypeclass opts)
    let op  = maybe [] (\x -> ["operations", x])    (optOperation opts)
    let obj = pkg ++ vsn ++ m ++ t ++ c ++ op

    res <- getInfos opts obj args2
    print res
