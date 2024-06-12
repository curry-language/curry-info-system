module CurryAnalysisInfrastructure.Structure where

import CurryAnalysisInfrastructure.Paths

import System.Directory

--- This functions generates a directory structure for the given package. Only directories are created.
initializePackageDirectory :: String -> IO ()
initializePackageDirectory pkg = do
    -- Save current directory
    current <- getCurrentDirectory
    home <- getHomeDirectory

    -- Jump to home directory
    setCurrentDirectory home

    {-
    tmp <- getDirectoryContents (home ++ "/.cpm/index")
    print tmp
    tmp <- doesDirectoryExist (home ++ "/.cpm/index")
    print tmp
    -}

    -- Create pkg directory
    path <- packagesPath
    createDirectoryIfMissing True (path ++ pkg)
    setCurrentDirectory (path ++ pkg)

    -- Get information about pkg
    i <- index
    b <- doesDirectoryExist (i ++ pkg)
    versions <- if b then do
                    contents <- getDirectoryContents (i ++ pkg)
                    return (drop 2 contents)
                else do
                    return []

    -- Create directories for each version
    mapM_ (initializeVersionDirectory pkg) versions

    -- Return to initial directory
    setCurrentDirectory current

--- This function generates a directory structure for a version of a package. Only directories are created.
initializeVersionDirectory :: String -> String -> IO ()
initializeVersionDirectory pkg vsn = do
    -- Save current directory
    current <- getCurrentDirectory

    -- Create version directory
    path <- versionsPath pkg
    createDirectoryIfMissing True (path ++ vsn)
    setCurrentDirectory (path ++ vsn)

    -- Get information about vsn in pkg
    ---

    -- Create directories for each module
    let mods = []
    mapM_ (initializeModuleDirectory pkg vsn) mods

    -- Return to initial directory
    setCurrentDirectory current

--- This function generates a directory structure for a module of a version of a package. Only directories are created.
initializeModuleDirectory :: String -> String -> String -> IO ()
initializeModuleDirectory pkg vsn m = do
    -- Save current directory
    current <- getCurrentDirectory

    -- Create module directory
    path <- modulesPath pkg vsn
    createDirectoryIfMissing True (path ++ m)
    setCurrentDirectory (path ++ m)

    -- Get information about m in vsn in pkg
    ---

    -- Create directories for each operation
    let ops = []
    mapM_ (initializeOperationDirectory pkg vsn m) ops

    -- Create directories for each type
    let types = []
    mapM_ (initializeTypeDirectory pkg vsn m) ops

    -- Create directories for each typeclass
    let typeclasses = []
    mapM_ (initializeTypeClassDirectory pkg vsn m) typeclasses

    -- Return to initial directory
    setCurrentDirectory current

--- This function generates a directory structure for a operation of a module of a version of a package. Only directories are created.
initializeOperationDirectory :: String -> String -> String -> String -> IO ()
initializeOperationDirectory pkg vsn m op = return ()

--- This function generates a directory structure for a type of a module of a version of a package. Only directories are created.
initializeTypeDirectory :: String -> String -> String -> String -> IO ()
initializeTypeDirectory pkg vsn m t = return ()

--- This function generates a directory structure for a typeclass of a module of a version of a package. Only directories are created.
initializeTypeClassDirectory :: String -> String -> String -> String -> IO ()
initializeTypeClassDirectory pkg vsn m tc = return ()