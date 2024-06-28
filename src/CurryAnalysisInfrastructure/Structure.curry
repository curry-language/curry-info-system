module CurryAnalysisInfrastructure.Structure where

import CurryAnalysisInfrastructure.Paths
import CurryAnalysisInfrastructure.Types

import System.Directory

setup :: IO ()
setup = do
    path <- root
    createDirectoryIfMissing True (path ++ "checkouts")
    createDirectoryIfMissing True (path ++ "packages")

--- This functions generates a directory structure for the given package. Only directories are created.
initializePackageDirectory :: CurryPackage -> IO ()
initializePackageDirectory x@(CurryPackage pkg) = do
    -- Save current directory
    current <- getCurrentDirectory
    home <- getHomeDirectory

    -- Jump to home directory
    setCurrentDirectory home

    -- Create pkg directory
    path <- getDirectoryPath x
    createDirectoryIfMissing True path
    setCurrentDirectory path

    -- Return to initial directory
    setCurrentDirectory current

--- This function generates a directory structure for a version of a package. Only directories are created.
initializeVersionDirectory :: CurryVersion -> IO ()
initializeVersionDirectory x@(CurryVersion pkg vsn) = do
    -- Save current directory
    current <- getCurrentDirectory

    -- Create version directory
    path <- getDirectoryPath x
    createDirectoryIfMissing True path
    setCurrentDirectory path

    -- Return to initial directory
    setCurrentDirectory current

--- This function generates a directory structure for a module of a version of a package. Only directories are created.
initializeModuleDirectory :: CurryModule -> IO ()
initializeModuleDirectory x@(CurryModule pkg vsn m) = do
    -- Save current directory
    current <- getCurrentDirectory

    -- Create module directory
    path <- getDirectoryPath x
    createDirectoryIfMissing True path
    setCurrentDirectory path

    -- Return to initial directory
    setCurrentDirectory current

--- This function generates a directory structure for a operation of a module of a version of a package. Only directories are created.
initializeOperationDirectory :: CurryOperation -> IO ()
initializeOperationDirectory (CurryOperation pkg vsn m op) = return ()

--- This function generates a directory structure for a type of a module of a version of a package. Only directories are created.
initializeTypeDirectory :: CurryType -> IO ()
initializeTypeDirectory (CurryType pkg vsn m t) = return ()

--- This function generates a directory structure for a typeclass of a module of a version of a package. Only directories are created.
initializeTypeClassDirectory :: CurryTypeclass -> IO ()
initializeTypeClassDirectory (CurryTypeclass pkg vsn m tc) = return ()