-----------------------------------------------------------------------------------------
--- This modules defines sets of options and operations to process them.
-----------------------------------------------------------------------------------------

module CurryInfo.Options where


import Control.Monad (when, unless, filterM)
import Data.List  (intercalate)
import Data.Maybe (catMaybes)
import Numeric (readNat)

import CurryInfo.Types
import CurryInfo.Configuration
import CurryInfo.Paths (getDirectoryPath, getJSONPath, packagesPath, root)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)
import CurryInfo.Checkout (checkouts, getCheckoutPath)
import CurryInfo.Helper (safeRead)

import System.Console.GetOpt
import System.Process (exitWith, system)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist, removeFile, removeDirectory)
import System.FilePath ((</>))

-- This operation returns a string representing the requests of the given
-- configuration using the given string as prefix.
printRequests :: String -> Configuration a -> String
printRequests s conf = s ++ "\n\n" ++ unlines (listRequests conf) ++ "\n\n"

-- The default options used by the tool.
defaultOptions :: Options
defaultOptions =
  Options 1 False 1 Nothing Nothing Nothing Nothing Nothing Nothing OutText
          False False False Nothing False False False

-- Options, with that nothing is printed by the tool.
silentOptions :: Options
silentOptions = defaultOptions { optForce = 1, optVerb = 0 }

testOptions :: Options
testOptions = defaultOptions { optVerb = 4 }

-- Options, that are used internally when querying specific information.
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
  when (optClean opts) (getObject opts >>= cleanObject opts >> exitWith 0)
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

-- This operation returns the requested object from the given options.
getObject :: Options -> IO (Maybe QueryObject)
getObject opts =
  case catMaybes [ extractOpt "package"   (optPackage opts)
                 , extractOpt "version"   (optVersion opts)
                 , extractOpt "module"    (optModule opts)
                 , extractOpt "type"      (optType opts)
                 , extractOpt "typeclass" (optTypeclass opts)
                 , extractOpt "operation" (optOperation opts) ] of
    [] -> return Nothing
    [("package", pkg)] -> return $ Just $ QueryPackage pkg
    [("package", pkg), ("version", vsn)] ->
      return $ Just $ QueryVersion pkg vsn
    [("package", pkg), ("version", vsn), ("module", m)] ->
      return $ Just $ QueryModule pkg vsn m
    [("package", pkg), ("version", vsn), ("module", m), ("type", t)] ->
      return $ Just $ QueryType pkg vsn m t
    [("package", pkg), ("version", vsn), ("module", m), ("typeclass", c)] ->
      return $ Just $ QueryTypeClass pkg vsn m c
    [("package", pkg), ("version", vsn), ("module", m), ("operation", o)] ->
      return $ Just $ QueryOperation pkg vsn m o
    obj -> do putStrLn $ "Options '" ++
                intercalate " " (map (\(t,v) -> "--" ++ t ++ "=" ++ v) obj) ++
                "'\ndo not match any request pattern!"
              exitWith 1
 where
  extractOpt :: String -> Maybe String -> Maybe (String, String)
  extractOpt tag = fmap (\x -> (tag, x))

-- This action deletes the locally stored information of the given object,
-- including subdirectories.
cleanObject :: Options -> Maybe QueryObject -> IO ()
cleanObject opts mbobj = case mbobj of
  Nothing -> cleanAll opts
  Just obj -> cleanJSON opts obj >> case obj of
    QueryPackage _             -> cleanDirectory opts obj
    QueryVersion pkg vsn       -> cleanDirectory opts obj >> cleanCheckout opts pkg vsn
    QueryModule pkg vsn _      -> cleanDirectory opts obj >> cleanCheckout opts pkg vsn
    QueryType pkg vsn _ _      -> cleanCheckout opts pkg vsn
    QueryTypeClass pkg vsn _ _ -> cleanCheckout opts pkg vsn
    QueryOperation pkg vsn _ _ -> cleanCheckout opts pkg vsn

-- This action deletes the json file containing the stored information of the given object.
cleanJSON :: Options -> QueryObject -> IO ()
cleanJSON opts obj = do
  getJSONPath obj >>= deleteFile opts

-- This action deletes the directory, in which the information of the given object art stored.
cleanDirectory :: Options -> QueryObject -> IO ()
cleanDirectory opts obj = do
  getDirectoryPath obj >>= deleteDirectory opts

-- This action deletes the checkout directory of the given package and version.
cleanCheckout :: Options -> Package -> Version -> IO ()
cleanCheckout opts pkg vsn = do
  getCheckoutPath pkg vsn >>= deleteDirectory opts

-- This operation puts quotation marks around the given string.
quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- This action deletes the entire directory used by this tool to store information locally.
cleanAll :: Options -> IO ()
cleanAll opts = root >>= deleteDirectory opts

-- This action deletes the given file using a boolean function to determine it
-- being the correct kind of file.
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

-- This action deletes the given directory, including all its contents.
deleteDirectory :: Options -> String -> IO ()
deleteDirectory = delete doesDirectoryExist "rm -rf"

-- This action deletes the given file.
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
       "verbosity level:\n0: quiet (no output besides the result)\n1: show status messages (default)\n2: show actions performed\n3: show all details (commands, files,...)"
  , Option "f" ["force"]
       (OptArg (maybe (checkForce 1) (safeReadNat checkForce)) "<n>")
       "force generation of requested information:\n0: no generation\n1: only generate when missing (default)\n2: always generate"
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
       (OptArg (\args opts -> opts { optOutput = maybe OutText read args })
               "<format>")
       "output format: Text (default), JSON, CurryTerm"
  , Option "" ["clean"]
       (NoArg (\opts -> opts { optClean = True }))
       "clean requested object or all information"
  , Option "" ["showall"]
       (NoArg (\opts -> opts { optShowAll = True }))
       "show all available information (no generation)"
  , Option "" ["server"]
       (NoArg (\opts -> opts { optServer = True }))
       "run the tool in server mode"
  , Option "" ["port"]
       (ReqArg (\args opts -> opts { optPort = safeRead args }) "<port>")
       "the port used in server mode"
  , Option "" ["alltypes"]
       (NoArg (\opts -> opts { optAllTypes = True }))
       "process requests for all types in module"
  , Option "" ["alltypeclasses"]
       (NoArg (\opts -> opts { optAllTypeclasses = True }))
       "process requests for all typeclasses in module"
  , Option "" ["alloperations"]
       (NoArg (\opts -> opts { optAllOperations = True }))
       "process requests for all operations in module"
  ]
  where
    safeReadNat opttrans s opts = case readNat s of
      [(n, "")] -> opttrans n opts
      _         -> error "Illegal number argument (try '-h' for help)"
    
    checkVerb n opts =
      if n >= 0 && n <= 3
        then opts { optVerb = n }
        else error "Illegal verbosity level (try '-h' for help)"
    checkForce n opts =
      if n >= 0 && n <= 2
        then opts { optForce = n }
        else error "Illegal force level (try '-h' for help)"
