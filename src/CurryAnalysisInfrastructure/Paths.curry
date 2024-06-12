module CurryAnalysisInfrastructure.Paths where

import System.Directory

index :: IO String
index = do
    home <- getHomeDirectory
    return (home ++ "/.cpm/index/")

root :: IO String
root = do
    home <- getHomeDirectory
    return (home ++ "/tmp" ++ "/.curryanalysis/")

packagesPath :: IO String
packagesPath = do 
    r <- root
    return (r ++ "packages/")

versionsPath :: String -> IO String
versionsPath pkg = do
    path <- packagesPath
    return (path ++ pkg ++ "/versions/")

modulesPath :: String -> String -> IO String
modulesPath pkg vsn = do
    path <- versionsPath pkg
    return (path ++ vsn ++ "/modules/")

typesPath :: String -> String -> String -> IO String
typesPath pkg vsn m = do 
    path <- modulesPath pkg vsn
    return (path ++ m ++ "/types/")

typeclassesPath :: String -> String -> String -> IO String
typeclassesPath pkg vsn m = do 
    path <- modulesPath pkg vsn
    return (path ++ m ++ "/typeclasses/")

operationsPath :: String -> String -> String -> IO String
operationsPath pkg vsn m = do 
    path <- modulesPath pkg vsn
    return (path ++ m ++ "/operations/")