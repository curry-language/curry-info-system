module CurryAnalysisInfrastructure.Options where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Configuration

import System.Console.GetOpt
import System.Process (exitWith)

import Numeric (readNat)

import Control.Monad (when, unless)

printRequests :: String -> Configuration a b -> String
printRequests s conf = s ++ "\n\n" ++ unlines (map (\(req, _) -> req ++ ":" ++ maybe "FAILED" id (findDescription req conf)) conf) ++ "\n\n"

defaultOptions :: Options
defaultOptions =
    Options 1 False 1 Nothing Nothing Nothing Nothing Nothing Nothing "text"

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

-- The options description.
options :: [OptDescr (Options -> Options)]
options =
    [ Option "h?" ["help"]
             (NoArg (\opts -> opts { optHelp = True }))
             "print help and exit"
    , Option "v" ["verbosity"]
             (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
             "verbosity level: NOT YET DEFINED"
    , Option "f" ["force"]
             --(NoArg (\opts -> opts { optForce = True }))
             (OptArg (maybe (checkForce 0) (safeReadNat checkForce)) "<n>")
             "force generation of missing requested information"
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
             "output format"
    ]
    where
        safeReadNat opttrans s opts = case readNat s of
            [(n, "")] -> opttrans n opts
            _ -> error "Illegal number argument (try '-h' for help)"
        
        checkVerb n opts = if n >= 0 && n <= 4
                                then opts { optVerb = n }
                                else error "Illegal verbosity level (try '-h' for help)"
        checkForce n opts = if n >= 0 && n <= 2
                                then opts { optForce = n }
                                else error "Illegal force level (try '-h' for help)"