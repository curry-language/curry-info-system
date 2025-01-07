------------------------------------------------------------------------------
--- This modules defines sets of options and operations to process them.
------------------------------------------------------------------------------

module CurryInfo.Options where


import Control.Monad ( when, unless, filterM )
import Data.List     ( intercalate )
import Data.Maybe    ( catMaybes )
import Numeric       ( readNat )

import CurryInfo.Types
import CurryInfo.Configuration
import CurryInfo.Paths     ( getDirectoryPath, getJSONPath, getRoot
                           , packagesPath )
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage )
import CurryInfo.Checkout  ( checkouts, getCheckoutPath )
import CurryInfo.Helper    ( quote, safeRead )

import System.Console.GetOpt
import System.Process (exitWith, system)
import System.CurryPath ( getPackageVersionOfModule )
import System.Directory ( getDirectoryContents, doesFileExist
                        , doesDirectoryExist, removeFile, removeDirectory )
import System.FilePath  ( (</>) )

-- This operation returns a string representing the requests of the given
-- configuration using the given string as prefix.
printRequests :: String -> Configuration a -> String
printRequests s conf = s ++ "\n\n" ++ unlines (listRequests conf) ++ "\n\n"

--- Use some coloring (from System.Console.ANSI.Codes) if color option is on.
withColor :: Options -> (String -> String) -> String -> String
withColor opts coloring = if optColor opts then coloring else id

--- The default options used by the tool.
defaultOptions :: Options
defaultOptions = 
  Options 1 False 1 Nothing Nothing Nothing Nothing Nothing Nothing OutText ""
          False False False False False Nothing False False False False

--- Options, with that nothing is printed by the tool (beyond the info results).
silentOptions :: Options
silentOptions = defaultOptions { optForce = 1, optVerb = 0 }

testOptions :: Options
testOptions = defaultOptions { optVerb = 4 }

--- Options, that are used internally when querying specific information.
queryOptions :: Options
queryOptions = silentOptions { optOutFormat = OutTerm }

-- This action takes the agruments given to the program and processes them.
-- If the help option is True, it prints the usage text and stops.
-- If some error happens, an error message is printed and the program stops.
processOptions :: String -> [String] -> IO (Options, [String])
processOptions banner argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
    (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  when (optCGI opts && (optServer opts || not (null (optOutFile opts))))
    (putStrLn
       "Options '--server' or '--output' not allowed in CGI mode!"
       >> exitWith 1)
  when (optClean opts) (getObject opts >>= cleanObject opts >> exitWith 0)
  return (opts, args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- The usage text of the program.
usageText :: String
usageText =
  usageInfo ("Usage: curry-info [options] <requests>\n") options ++
  "\nRequests for different kinds of entities:\n\n" ++
  printRequests "Package"   packageConfiguration ++
  printRequests "Version"   versionConfiguration ++
  printRequests "Module"    moduleConfiguration ++
  printRequests "Type"      typeConfiguration ++
  printRequests "Class"     classConfiguration ++
  printRequests "Operation" operationConfiguration

-- This operation returns the requested object from the given options.
getObject :: Options -> IO (Maybe QueryObject)
getObject opts = do
  let entopts = catMaybes [ extractOpt "package"   (optPackage opts)
                 , extractOpt "version"   (optVersion opts)
                 , extractOpt "module"    (optModule opts)
                 , extractOpt "type"      (optType opts)
                 , extractOpt "class"     (optClass opts)
                 , extractOpt "operation" (optOperation opts) ]
  case entopts of
    [] -> return Nothing
    [("package", pkg)] -> return $ Just $ QueryPackage pkg
    [("package", pkg), ("version", vsn)] ->
      return $ Just $ QueryVersion pkg vsn
    [("package", pkg), ("version", vsn), ("module", m)] ->
      return $ Just $ QueryModule pkg vsn m
    [("package", pkg), ("version", vsn), ("module", m), ("type", t)] ->
      return $ Just $ QueryType pkg vsn m t
    [("package", pkg), ("version", vsn), ("module", m), ("class", c)] ->
      return $ Just $ QueryClass pkg vsn m c
    [("package", pkg), ("version", vsn), ("module", m), ("operation", o)] ->
      return $ Just $ QueryOperation pkg vsn m o
    [("module", m), ("type", t)] ->
      getPackageVersionOfModule m >>=
        maybe (exitWithOptError entopts)
              (\(pkg,vsn) -> do printDetailMessage opts $ addPkgVsnMsg pkg vsn
                                return $ Just $ QueryType pkg vsn m t)
    [("module", m), ("class", c)] ->
      getPackageVersionOfModule m >>=
        maybe (exitWithOptError entopts)
              (\(pkg,vsn) -> do printDetailMessage opts $ addPkgVsnMsg pkg vsn
                                return $ Just $ QueryClass pkg vsn m c)
    [("module", m), ("operation", o)] ->
      getPackageVersionOfModule m >>=
        maybe (exitWithOptError entopts)
              (\(pkg,vsn) -> do printDetailMessage opts $ addPkgVsnMsg pkg vsn
                                return $ Just $ QueryOperation pkg vsn m o)
    _ -> exitWithOptError entopts
 where
  addPkgVsnMsg pkg vsn =
    "Add options '--package=" ++ pkg ++ " --version=" ++ vsn ++ "'"

  exitWithOptError eopts = do
    putStrLn $ "Options '" ++
               intercalate " " (map (\(t,v) -> "--" ++ t ++ "=" ++ v) eopts) ++
               "'\ndo not match any request pattern!"
    exitWith 1

  extractOpt :: String -> Maybe String -> Maybe (String, String)
  extractOpt tag = fmap (\x -> (tag, x))

-- This action deletes the locally stored information of the given object,
-- including subdirectories.
cleanObject :: Options -> Maybe QueryObject -> IO ()
cleanObject opts mbobj = case mbobj of
  Nothing  -> checkNotCGI >> cleanAll opts
  Just obj -> cleanJSON opts obj >> case obj of
    QueryPackage _             -> checkNotCGI >> cleanDirectory opts obj
    QueryVersion pkg vsn       -> cleanDirectory opts obj >>
                                  cleanCheckout opts pkg vsn
    QueryModule pkg vsn _      -> cleanDirectory opts obj >>
                                  cleanCheckout opts pkg vsn
    QueryType pkg vsn _ _      -> cleanCheckout opts pkg vsn
    QueryClass pkg vsn _ _     -> cleanCheckout opts pkg vsn
    QueryOperation pkg vsn _ _ -> cleanCheckout opts pkg vsn
 where
  checkNotCGI =
    when (optCGI opts)
      (putStrLn
         "Option '--clean' for all package versions not allowed in CGI mode!"
       >> exitWith 1)

-- This action deletes the json file containing the stored information
-- of the given object.
cleanJSON :: Options -> QueryObject -> IO ()
cleanJSON opts obj = do
  getJSONPath obj >>= deleteFile opts

-- This action deletes the directory, in which the information of the
-- given object is stored.
cleanDirectory :: Options -> QueryObject -> IO ()
cleanDirectory opts obj = getDirectoryPath obj >>= deleteDirectory opts

-- This action deletes the checkout directory of the given package and version.
cleanCheckout :: Options -> Package -> Version -> IO ()
cleanCheckout opts pkg vsn = getCheckoutPath pkg vsn >>= deleteDirectory opts

-- This action deletes the entire directory used by this tool to store
-- information locally.
cleanAll :: Options -> IO ()
cleanAll opts = getRoot >>= deleteDirectory opts

-- This action deletes the given file using a Boolean function to determine it
-- being the correct kind of file.
delete :: (String -> IO Bool) -> String -> Options -> String -> IO ()
delete check cmd opts path = do
  exists <- check path
  when exists $ do
    let cmd' = cmd ++ " " ++ quote path
    exitCode <- system cmd'
    printDetailMessage opts $ case exitCode of
      127 -> "Command '" ++ cmd' ++ "' could not be found."
      126 -> "Command '" ++ cmd' ++ "' was not an executable."
      0   -> "Command '" ++ cmd' ++ "' finished successfully."
      _   -> "Command '" ++ cmd' ++ "' failed with exist code '" ++
             show exitCode ++ "'."

-- This action deletes the given directory, including all its contents.
deleteDirectory :: Options -> String -> IO ()
deleteDirectory opts dir = do
  printDebugMessage opts $ "Deleting directory " ++ quote dir ++ "..."
  delete doesDirectoryExist "rm -rf" opts dir

-- This action deletes the given file.
deleteFile :: Options -> String -> IO ()
deleteFile opts dir = do
  printDebugMessage opts $ "Deleting file " ++ quote dir ++ "..."
  delete doesFileExist "rm -f" opts dir

-- The options description.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]
       (NoArg (\opts -> opts { optHelp = True }))
       "print help and exit"
  , Option "v" ["verbosity"]
       (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
       "verbosity level:\n0: quiet (no output besides info result)\n1: show status messages (default)\n2: show actions performed (same as '-v')\n3: show all details (commands, files,...)"
  , Option "q" ["quiet"]
       (NoArg (\opts -> opts { optVerb = 0 }))
       "run quietly (same as `--quiet`)"
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
  , Option "c" ["class"]
       (ReqArg (\args opts -> opts { optClass = Just args }) "<c>")
       "requested type class"
  , Option "o" ["operation"]
       (ReqArg (\args opts -> opts { optOperation = Just args }) "<o>")
       "requested operation"
  , Option "" ["alltypes"]
       (NoArg (\opts -> opts { optAllTypes = True }))
       "process requests for all types in module"
  , Option "" ["allclasses"]
       (NoArg (\opts -> opts { optAllClasses = True }))
       "process requests for all type classes in module"
  , Option "" ["alloperations"]
       (NoArg (\opts -> opts { optAllOperations = True }))
       "process requests for all operations in module"
  , Option "" ["format"]
       (ReqArg (\args opts -> opts { optOutFormat =
                                       maybe OutText id (safeRead args) })
          "<format>")
       "output format: Text (default), JSON, CurryTerm"
  , Option "" ["output"]
       (ReqArg (\arg opts -> opts { optOutFile = arg }) "<f>")
       "write results to file <f> (default: stdout)"
  , Option "" ["clean"]
       (NoArg (\opts -> opts { optClean = True }))
       "clean requested object or all information"
  , Option "" ["color"]
       (NoArg (\opts -> opts { optColor = True }))
       "use colors in text output"
  , Option "" ["showall"]
       (NoArg (\opts -> opts { optShowAll = True }))
       "show all available information (no generation)"
  , Option "" ["cgi"] -- will be processed in `Main.main`
       (NoArg (\opts -> opts { optCGI = True }))
       "run the tool in CGI mode"
  , Option "" ["server"]
       (NoArg (\opts -> opts { optServer = True }))
       "run the tool in server mode"
  , Option "" ["port"]
       (ReqArg (\args opts -> opts { optPort = safeRead args }) "<port>")
       "the port used in server mode"
  , Option "" ["update"]
       (NoArg (\opts -> opts { optUpdate = True }))
       "update package index (by 'cypm update')"
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
