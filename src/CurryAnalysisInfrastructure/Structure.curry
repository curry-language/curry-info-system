module CurryAnalysisInfrastructure.Structure where

import System.Directory

initializePackageDirectory :: String -> IO ()
initializePackageDirectory pkg = do
    -- Save current directory
    current <- getCurrentDirectory
    home <- getHomeDirectory

    -- Jump to home directory
    setCurrentDirectory home

    -- Get information about pkg
    i <- index
    b <- doesDirectoryExist (i ++ pkg)
    tmp <- getDirectoryContents (home ++ "/.cpm/index")
    print tmp
    tmp <- doesDirectoryExist (home ++ "/.cpm/index")
    print tmp
    versions <- if b then do
                    contents <- getDirectoryContents (i ++ pkg)
                    return (drop 2 contents)
                else do
                    return []

    -- Create pkg directory
    r <- root
    createDirectoryIfMissing True (r ++ pkg)
    setCurrentDirectory (r ++ pkg)

    -- Create directories for each version
    mapM_ (initializeVersionDirectory pkg) versions

    -- Return to initial directory
    setCurrentDirectory current

initializeVersionDirectory :: String -> String -> IO ()
initializeVersionDirectory pkg vsn = do
    -- Save current directory
    current <- getCurrentDirectory

    -- Get information about vsn in pkg
    ---

    -- Create version directory
    let r = "versions/"
    createDirectoryIfMissing True (r ++ vsn)
    setCurrentDirectory (r ++ vsn)

    -- Create directories for each module
    let mods = []
    mapM_ (initializeModuleDirectory pkg vsn) mods

    -- Return to initial directory
    setCurrentDirectory current

initializeModuleDirectory :: String -> String -> String -> IO ()
initializeModuleDirectory pkg vsn m = do
    -- Save current directory
    current <- getCurrentDirectory

    -- Get information about m in vsn in pkg
    ---

    -- Create module directory
    let r = "modules/"
    createDirectoryIfMissing True (r ++ m)
    setCurrentDirectory (r ++ m)

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

initializeOperationDirectory :: String -> String -> String -> String -> IO ()
initializeOperationDirectory pkg vsn m op = return ()

initializeTypeDirectory :: String -> String -> String -> String -> IO ()
initializeTypeDirectory pkg vsn m t = return ()

initializeTypeClassDirectory :: String -> String -> String -> String -> IO ()
initializeTypeClassDirectory pkg vsn m tc = return ()