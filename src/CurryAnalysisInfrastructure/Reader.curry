module CurryAnalysisInfrastructure.Reader where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.JParser (jparse, JParser)
import CurryAnalysisInfrastructure.Paths (getJSONPath, Path)

import JSON.Parser (parseJSON)

import System.Directory (doesFileExist)

type Reader a b = a -> IO (Maybe [b])

readInformation :: (Path a, JParser b) => Reader a b
readInformation input = do
    path <- getJSONPath input
    b <- doesFileExist path
    case b of
        False -> return Nothing
        True -> do
            jtext <- readFile path
            return $ parseJSON jtext >>= jparse

{-
readPackageInformation :: Reader CurryPackage PackageInformation
readPackageInformation x@(CurryPackage pkg) = do
    path <- getJSONPath x
    b <- doesFileExist path
    case b of
        False -> return Nothing
        True -> do
            jtext <- readFile path
            return $ parseJSON jtext >>= jparse

readVersionInformation :: Reader CurryVersion VersionInformation
readVersionInformation x@(CurryVersion pkg vsn) = do
    path <- getJSONPath x
    b <- doesFileExist path
    case b of
        False -> return Nothing
        True -> do
            jtext <- readFile path
            return $ parseJSON jtext >>= jparse

readModuleInformation :: Reader CurryModule ModuleInformation
readModuleInformation x@(CurryModule pkg vsn m) = do
    path <- getJSONPath x
    b <- doesFileExist path
    case b of
        False -> return Nothing
        True -> do
            jtext <- readFile path
            return $ parseJSON jtext >>= jparse
-}