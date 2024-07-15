module CurryAnalysisInfrastructure.Options where

import System.Console.GetOpt
import System.Process (exitWith)

import Numeric (readNat)

import Control.Monad (when, unless)

data Options = Options
    { optVerb :: Int
    , optHelp :: Bool
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
    Options 1 False Nothing Nothing Nothing Nothing Nothing Nothing

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
    , Option "p" ["package"]
             (OptArg (\args opts -> opts { optPackage = args }) "<pkg>")
             "requested package"
    , Option "x" ["version"]
             (OptArg (\args opts -> opts { optVersion = args }) "<vsn>")
             "requested version"
    , Option "m" ["module"]
             (OptArg (\args opts -> opts { optModule = args }) "<mod>")
             "requested module"
    , Option "t" ["type"]
             (OptArg (\args opts -> opts { optType = args }) "<mod>")
             "requested type"
    , Option "c" ["typeclass"]
             (OptArg (\args opts -> opts { optTypeclass = args }) "<mod>")
             "requested typeclass"
    , Option "o" ["operation"]
             (OptArg (\args opts -> opts { optOperation = args }) "<mod>")
             "requested operation"
    ]
    where
        safeReadNat opttrans s opts = case readNat s of
            [(n, "")] -> opttrans n opts
            _ -> error "Illegal number argument (try '-h' for help)"
        
        checkVerb n opts = if n >= 0 && n <= 4
                                then opts { optVerb = n }
                                else error "Illegal verbosity level (try '-h' for help)"