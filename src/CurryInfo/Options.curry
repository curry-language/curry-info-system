module CurryInfo.Options where

import CurryInfo.Types
import CurryInfo.Configuration
import CurryInfo.Paths (Path, getDirectoryPath, getJSONPath, packagesPath)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)

import System.Console.GetOpt
import System.Process (exitWith)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist, removeFile, removeDirectory)
import System.FilePath ((</>))

import Numeric (readNat)

import Control.Monad (when, unless, filterM)

import Data.Maybe (catMaybes)

printRequests :: String -> [RegisteredRequest a] -> String
printRequests s conf = s ++ "\n\n" ++ unlines (map (\rreq -> request rreq ++ ":" ++ description' rreq) conf) ++ "\n\n"

defaultOptions :: Options
defaultOptions =
    Options 1 False 1 Nothing Nothing Nothing Nothing Nothing Nothing "text" False

silentOptions :: Options
silentOptions = defaultOptions { optForce = 1, optVerb = 0 }

testOptions :: Options
testOptions = defaultOptions { optVerb = 4 }

-- This action takes the agrument given to the program and processes the arguments.
-- If the help option is True, it prints the usage text and stops.
-- If some error happens, an error message is printed and the program stops.
processOptions :: String -> [String] -> IO (Options, [String])
processOptions banner argv = do
    let (funopts, args, opterrors) = getOpt Permute options argv
        opts = foldl (flip id) defaultOptions funopts
    unless (null opterrors)
           (putStr (unlines opterrors) >> printUsage >> exitWith 1)
    when (optHelp opts) (printUsage >> exitWith 0)
    when (optClean opts) (cleanObject opts (getObject opts) >> exitWith 0)
    return (opts, args)
    where
        printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- The usage text of the program.
usageText :: String
usageText =
    usageInfo ("Usage: tool [options] <requests>\n") options ++
    "\nRequests:\n\n" ++
    printRequests "Package" packageConfiguration ++
    printRequests "Version" versionConfiguration ++
    printRequests "Module" moduleConfiguration ++
    printRequests "Type" typeConfiguration ++
    printRequests "Typeclass" typeclassConfiguration ++
    printRequests "Operation" operationConfiguration

getObject :: Options -> [(String, String)]
getObject opts =
        let pkg = extractOpt "packages"     (optPackage opts)
            vsn = extractOpt "versions"     (optVersion opts)
            m   = extractOpt "modules"      (optModule opts)
            t   = extractOpt "types"        (optType opts)
            c   = extractOpt "typeclasses"  (optTypeclass opts)
            op  = extractOpt "operations"   (optOperation opts)
        in catMaybes [pkg, vsn, m, t, c, op]
    where
        extractOpt :: String -> Maybe String -> Maybe (String, String)
        extractOpt tag = fmap (\x -> (tag, x))

cleanObject :: Options -> [(String, String)] -> IO ()
cleanObject opts obj = do
        case obj of
            [] -> cleanAll
            [("packages", pkg)] -> let x = CurryPackage pkg in cleanJSON x >> cleanDirectory x
            [("packages", pkg), ("versions", vsn)] -> let x = CurryVersion pkg vsn in cleanJSON x >> cleanDirectory x
            [("packages", pkg), ("versions", vsn), ("modules", m)] -> let x = CurryModule pkg vsn m in cleanJSON x >> cleanDirectory x
            [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)] -> let x = CurryType pkg vsn m t in cleanJSON x
            [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)] -> let x = CurryTypeclass pkg vsn m c in cleanJSON x
            [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)] -> let x = CurryOperation pkg vsn m o in cleanJSON x
            _ -> printDebugMessage opts $ show obj ++ " does not match any pattern"
    where
        cleanJSON :: Path a => a -> IO ()
        cleanJSON obj' = do
            path <- getJSONPath obj'
            b <- doesFileExist path
            case b of
                False -> do
                    printDebugMessage opts $ "json file does not exist: " ++ path ++ "\nNot cleaning up json file."
                    return ()
                True -> do
                    printDebugMessage opts $ "json file exists. Cleaning up json file."
                    removeFile path

        cleanDirectory :: Path a => a -> IO ()
        cleanDirectory obj' = do
            path <- getDirectoryPath obj'
            b <- doesDirectoryExist path
            case b of
                False -> do
                    printDebugMessage opts $ "directory does not exist: " ++ path ++ "\nNot cleaning up directory."
                    return ()
                True -> do
                    printDebugMessage opts $ "directory exists. Cleaning up directory."
                    deleteDirectory path

        cleanAll :: IO ()
        cleanAll = do
            path <- packagesPath
            b <- doesDirectoryExist path
            case b of
                False -> do
                    printDebugMessage opts $ "directory does not exist: " ++ path ++ "\nNot cleaning up directory."
                    return ()
                True -> do
                    printDebugMessage opts $ "directory exists. Cleaning up directory."
                    deleteDirectory path
        
        deleteDirectory :: String -> IO ()
        deleteDirectory path = do
            printDebugMessage opts $ "Deleting directory: " ++ path

            contents <- fmap ((map (\p -> path </> p)) . (filter (\p -> p /= "." && p /= ".."))) (getDirectoryContents path)
            printDebugMessage opts $ "Found contents: " ++ show contents

            dirs <- filterM doesDirectoryExist contents
            printDebugMessage opts $ "Subdirectories found: " ++ show dirs

            files <- filterM doesFileExist contents
            printDebugMessage opts $ "Files found: " ++ show files

            printDebugMessage opts "Deleting subdirectories..."
            mapM deleteDirectory dirs

            printDebugMessage opts "Deleting files..."
            mapM removeFile files

            printDebugMessage opts "Deleting directory..."
            removeDirectory path

            printDebugMessage opts $ "Finished deleting directory: " ++ path

-- The options description.
options :: [OptDescr (Options -> Options)]
options =
    [ Option "h?" ["help"]
             (NoArg (\opts -> opts { optHelp = True }))
             "print help and exit"
    , Option "v" ["verbosity"]
             (OptArg (maybe (checkVerb 1) (safeReadNat checkVerb)) "<n>")
             "verbosity level\n\t0: quiet (no output besides the result)\n\t1: Show status messages (like what request is being processed) (default)\n\t2: Show actions performed (like checkout a package version and running an analysis\n\t3: Show all detailes (like commands to be invoked and what files to update)"
    , Option "f" ["force"]
             (OptArg (maybe (checkForce 1) (safeReadNat checkForce)) "<n>")
             "force generation of missing requested information\n\t0: No generation\n\t1: Only generate when missing (default)\n\t2: Always generate"
    , Option "p" ["package"]
             (ReqArg (\args opts -> opts { optPackage = Just args }) "<pkg>")
             "requested package"
    , Option "x" ["version"]
             (ReqArg (\args opts -> opts { optVersion = Just args }) "<vsn>")
             "requested version"
    , Option "m" ["module"]
             (ReqArg (\args opts -> opts { optModule = Just args }) "<mod>")
             "requested module"
    , Option "t" ["type"]
             (ReqArg (\args opts -> opts { optType = Just args }) "<t>")
             "requested type"
    , Option "c" ["typeclass"]
             (ReqArg (\args opts -> opts { optTypeclass = Just args }) "<c>")
             "requested typeclass"
    , Option "o" ["operation"]
             (ReqArg (\args opts -> opts { optOperation = Just args }) "<o>")
             "requested operation"
    , Option "" ["output"]
             (OptArg (\args opts -> opts { optOutput = maybe "text" id args }) "<format>")
             "output format: text (default), json"
    , Option "" ["clean"]
             (NoArg (\opts -> opts { optClean = True }))
             "clean up the requested object or all information and exit"
    ]
    where
        safeReadNat opttrans s opts = case readNat s of
            [(n, "")] -> opttrans n opts
            _ -> error "Illegal number argument (try '-h' for help)"
        
        checkVerb n opts = if n >= 0 && n <= 3
                                then opts { optVerb = n }
                                else error "Illegal verbosity level (try '-h' for help)"
        checkForce n opts = if n >= 0 && n <= 2
                                then opts { optForce = n }
                                else error "Illegal force level (try '-h' for help)"
