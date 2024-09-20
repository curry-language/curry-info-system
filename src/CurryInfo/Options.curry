module CurryInfo.Options where

import CurryInfo.Types
import CurryInfo.Configuration
import CurryInfo.Paths (Path, getDirectoryPath, getJSONPath, packagesPath, root)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Checkout (checkouts, getCheckoutPath)

import System.Console.GetOpt
import System.Process (exitWith, system)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist, removeFile, removeDirectory)
import System.FilePath ((</>))

import Numeric (readNat)

import Control.Monad (when, unless, filterM)

import Data.Maybe (catMaybes)

printRequests :: String -> [RegisteredRequest a] -> String
printRequests s conf = s ++ "\n\n" ++ unlines (map (\rreq -> request rreq ++ ":" ++ description' rreq) conf) ++ "\n\n"

defaultOptions :: Options
defaultOptions =
    Options 1 False 1 Nothing Nothing Nothing Nothing Nothing Nothing OutText False False

silentOptions :: Options
silentOptions = defaultOptions { optForce = 1, optVerb = 0 }

testOptions :: Options
testOptions = defaultOptions { optVerb = 4 }

queryOptions :: Options
queryOptions = silentOptions { optOutput = OutTerm }

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
            [] -> cleanAll opts
            [("packages", pkg)] -> let x = CurryPackage pkg in cleanJSON opts x >> cleanDirectory opts x
            [("packages", pkg), ("versions", vsn)] -> let x = CurryVersion pkg vsn in cleanJSON opts x >> cleanDirectory opts x >> cleanCheckout opts pkg vsn
            [("packages", pkg), ("versions", vsn), ("modules", m)] -> let x = CurryModule pkg vsn m in cleanJSON opts x >> cleanDirectory opts x >> cleanCheckout opts pkg vsn
            [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)] -> let x = CurryType pkg vsn m t in cleanJSON opts x >> cleanCheckout opts pkg vsn
            [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)] -> let x = CurryTypeclass pkg vsn m c in cleanJSON opts x >> cleanCheckout opts pkg vsn
            [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)] -> let x = CurryOperation pkg vsn m o in cleanJSON opts x >> cleanCheckout opts pkg vsn
            _ -> printDebugMessage opts $ show obj ++ " does not match any pattern"
   -- where
cleanJSON :: Path a => Options -> a -> IO ()
cleanJSON opts obj' = do
    getJSONPath obj' >>= deleteFile opts

cleanDirectory :: Path a => Options -> a -> IO ()
cleanDirectory opts obj' = do
    getDirectoryPath obj' >>= deleteDirectory opts

cleanCheckout :: Options -> Package -> Version -> IO ()
cleanCheckout opts pkg vsn = do
    getCheckoutPath pkg vsn >>= deleteDirectory opts
        
quote :: String -> String
quote s = "\"" ++ s ++ "\""

cleanAll :: Options -> IO ()
cleanAll opts = do
    packagesPath >>= deleteDirectory opts
    checkouts >>= deleteDirectory opts
    root >>= deleteDirectory opts

delete :: (String -> IO Bool) -> String -> Options -> String -> IO ()
delete check cmd opts path = do
    exists <- check path
    when exists $ do
        let cmd' = cmd ++ " " ++ quote path
        exitCode <- system cmd'
        case exitCode of
            127 -> do
                printDetailMessage opts $ "Command '" ++ cmd' ++ "' could not be found."
            126 -> do
                printDetailMessage opts $ "Command '" ++ cmd' ++ "' was not an executable."
            0 -> do
                printDetailMessage opts $ "Command '" ++ cmd' ++ "' finished successfully."
            _ -> do
                printDetailMessage opts $ "Command '" ++ cmd' ++ "' failed with exist code '" ++ show exitCode ++ "'."

deleteDirectory :: Options -> String -> IO ()
deleteDirectory = delete doesDirectoryExist "rm -rf"

deleteFile :: Options -> String -> IO ()
deleteFile = delete doesFileExist "rm -f"

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
             (OptArg (\args opts -> opts { optOutput = maybe OutText read args }) "<format>")
             "output format: Text (default), JSON, CurryTerm"
    , Option "" ["clean"]
             (NoArg (\opts -> opts { optClean = True }))
             "clean up the requested object or all information and exit"
    , Option "" ["showall"]
             (NoArg (\opts -> opts { optShowAll = True }))
             "show all currently available information (without generating)"
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
