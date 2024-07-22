module CurryAnalysisInfrastructure.Options where

import System.Console.GetOpt
import System.Process (exitWith)

import Numeric (readNat)

import Control.Monad (when, unless)

data Options = Options
    { optVerb :: Int
    , optHelp :: Bool
    --, optForce :: Bool
    , optForce :: Int
    , optPackage :: Maybe String
    , optVersion :: Maybe String
    , optModule :: Maybe String
    , optType :: Maybe String
    , optTypeclass :: Maybe String
    , optOperation :: Maybe String
    }
    deriving Show 

defaultOptions :: Options
defaultOptions =
    Options 1 False 0 Nothing Nothing Nothing Nothing Nothing Nothing

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

usageText :: String
usageText = usageInfo ("Usage: tool [options] <requests>\n") options

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
             (ReqArg (\args opts -> opts { optType = Just args }) "<mod>")
             "requested type"
    , Option "c" ["typeclass"]
             (ReqArg (\args opts -> opts { optTypeclass = Just args }) "<mod>")
             "requested typeclass"
    , Option "o" ["operation"]
             (ReqArg (\args opts -> opts { optOperation = Just args }) "<mod>")
             "requested operation"
    ]
    where
        safeReadNat opttrans s opts = case readNat s of
            [(n, "")] -> opttrans n opts
            _ -> error "Illegal number argument (try '-h' for help)"
        
        checkVerb n opts = if n >= 0 && n <= 4
                                then opts { optVerb = n }
                                else error "Illegal verbosity level (try '-h' for help)"
        checkForce n opts = if n >= 0 && n <= 1
                                then opts { optForce = n }
                                else error "Illegal force level (try '-h' for help)"

fullVerbosity :: Options -> Bool
fullVerbosity opts = optVerb opts >= 4