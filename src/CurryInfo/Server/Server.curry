-----------------------------------------------------------------------------------------
--- This modules defines operations to run the tool in server mode.
-----------------------------------------------------------------------------------------

module CurryInfo.Server.Server where

import CurryInfo.Types
import CurryInfo.Options
import CurryInfo.Verbosity
import CurryInfo.Information (getInfos, printResult)
import CurryInfo.Configuration
import CurryInfo.Helper (safeRead)
import CurryInfo.Server.Configuration

import System.Process (system, sleep)
import System.IO (Handle, hIsEOF, hClose, hFlush, hPutStrLn, hGetChar)

import Network.Socket (Socket(..), listenOn, listenOnFresh
            , close, waitForSocketAccept)

type Force = Int

data SingleOrAll = Single | AllTypes | AllClasses | AllOperations

data InfoServerMessage
  = GetRequests (Maybe String)
  | GetCommands
  | RequestPackageInformation (Maybe OutFormat) (Maybe Force) Package [String]
  | RequestVersionInformation (Maybe OutFormat) (Maybe Force) Package Version [String]
  | RequestModuleInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | RequestTypeInformation (Maybe OutFormat) (Maybe Force) Package Version Module Type [String]
  | RequestClassInformation (Maybe OutFormat) (Maybe Force) Package Version Module Class [String]
  | RequestOperationInformation (Maybe OutFormat) (Maybe Force) Package Version Module Operation [String]
  | RequestAllTypesInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | RequestAllClassesInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | RequestAllOperationsInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | StopServer
  | ParseError

-- This action starts the server.
startServer :: Options -> IO ()
startServer opts = do
  printStatusMessage opts "Start Server"
  (port1, socket1) <- maybe listenOnFresh
            (\p -> listenOn p >>= \s -> return (p, s))
            (optPort opts)
  putStrLn ("Server Port: " ++ show port1)
  storeServerPortNumber port1
  serverLoop opts socket1

-- This action is the main server loop.
serverLoop :: Options -> Socket -> IO ()
serverLoop opts socket1 = do
  connection <- waitForSocketAccept socket1 waitTime
  case connection of
    Just (_, handle) -> serverLoopOnHandle opts socket1 handle
    Nothing -> do
      putStrLn "serverLoop: connection error: time out in waitForSocketAccept"
      sleep 1
      serverLoop opts socket1

-- This action is the main loop of handling communication after successfully connecting.
serverLoopOnHandle :: Options -> Socket -> Handle -> IO ()
serverLoopOnHandle opts socket1 handle = do
  eof <- hIsEOF handle
  if eof
    then do
      hClose handle
      printDetailMessage opts "SERVER connection: eof"
      serverLoop opts socket1
    else do
      string <- hGetLineUntilEOF handle
      printDetailMessage opts $ "SERVER got message: " ++ string
      case parseServerMessage string of
        ParseError -> do
          sendServerError opts handle $ "Illegal message received: " ++ string
          serverLoopOnHandle opts socket1 handle
        GetRequests mobj -> do
          sendRequestNamesAndFormats opts handle mobj
          serverLoopOnHandle opts socket1 handle
        GetCommands -> do
          let msg =
                [ "GetRequests | GetRequests <obj>"
                , "GetCommands"
                , "RequestPackageInformation <outform> <force> <pkg> <reqs>"
                , "RequestVersionInformation <outform> <force> <pkg> <vsn> <reqs>"
                , "RequestModuleInformation <outform> <force> <pkg> <vsn> <mod>  <reqs>"
                , "RequestTypeInformation <outform> <force> <pkg> <vsn> <mod> <t> <reqs>"
                , "RequestClassInformation <outform> <force> <pkg> <vsn> <mod> <tc> <reqs>"
                , "RequestOperationInformation <outform> <force> <pkg> <vsn> <mod> <op> <reqs>"
                , "RequestAllTypesInformation <outform> <force> <pkg> <vsn> <mod> <reqs"
                , "RequestAllClassesInformation <outform> <force> <pkg> <vsn> <mod> <reqs"
                , "RequestAllOperationsInformation <outform> <force> <pkg> <vsn> <mod> <reqs"
                , "StopServer"
                ]
          sendServerResult handle (unlines msg)
          serverLoopOnHandle opts socket1 handle
        RequestPackageInformation moutform mforce pkg reqs -> 
          requestInformation moutform mforce Single (QueryPackage pkg) reqs
        RequestVersionInformation moutform mforce pkg vsn reqs -> 
          requestInformation moutform mforce Single (QueryVersion pkg vsn) reqs
        RequestModuleInformation moutform mforce pkg vsn m reqs -> 
          requestInformation moutform mforce Single (QueryModule pkg vsn m) reqs
        RequestTypeInformation moutform mforce pkg vsn m t reqs -> 
          requestInformation moutform mforce Single (QueryType pkg vsn m t) reqs
        RequestClassInformation moutform mforce pkg vsn m c reqs -> 
          requestInformation moutform mforce Single (QueryClass pkg vsn m c) reqs
        RequestOperationInformation moutform mforce pkg vsn m o reqs -> 
          requestInformation moutform mforce Single (QueryOperation pkg vsn m o) reqs
        RequestAllTypesInformation moutform mforce pkg vsn m reqs ->
          requestInformation moutform mforce AllTypes (QueryModule pkg vsn m) reqs
        RequestAllClassesInformation moutform mforce pkg vsn m reqs ->
          requestInformation moutform mforce AllClasses (QueryModule pkg vsn m) reqs
        RequestAllOperationsInformation moutform mforce pkg vsn m reqs ->
          requestInformation moutform mforce AllOperations (QueryModule pkg vsn m) reqs
        StopServer -> do
          sendServerResult handle ""
          hClose handle
          close socket1
          printStatusMessage opts "Stop Server"
          removeServerPortNumber
 where
  sendResult resultstring = do
    printDebugMessage opts $ "Formatted result:\n" ++ resultstring
    sendServerResult handle resultstring
    serverLoopOnHandle opts socket1 handle
  
  sendRequestError err = do
    sendServerError opts handle ("Error in information server: " ++ show err)
    serverLoopOnHandle opts socket1 handle
  
  requestInformation moutform mforce singleOrAll obj reqs = case (moutform, mforce) of
    (Nothing, Nothing) -> sendRequestError "Given output format and given force value do not exist"
    (Nothing, Just _ ) -> sendRequestError "Given output format does not exist"
    (Just _ , Nothing) -> sendRequestError "Given force value does not exist"
    (Just outform, Just force) -> catch
      (let giopts = silentOptions { optForce = force, optOutFormat = outform }
       in getInfos (change singleOrAll giopts) obj reqs
            >>= printResult giopts >>= sendResult)
      sendRequestError
  
  change singleOrAll slopts = case singleOrAll of
    Single        -> slopts
    AllTypes      -> slopts { optAllTypes = True }
    AllClasses    -> slopts { optAllClasses = True }
    AllOperations -> slopts { optAllOperations = True }

-- This action sends a result string over the given handle.
sendServerResult :: Handle -> String -> IO ()
sendServerResult handle resultstring = do
  let resultlines = lines resultstring
  hPutStrLn handle ("ok " ++ show (length resultlines))
  hPutStrLn handle (unlines resultlines)
  hFlush handle

-- This action sends an error string over the given handle.
sendServerError :: Options -> Handle -> String -> IO ()
sendServerError opts handle errstring = do
  printStatusMessage opts errstring
  hPutStrLn handle $ "ERROR: " ++ errstring
  hFlush handle

-- This action reads from a handle until it reaches EOF.
hGetLineUntilEOF  :: Handle -> IO String
hGetLineUntilEOF h = do
  eof <- hIsEOF h
  if eof
   then return ""
   else do c <- hGetChar h
           if c=='\n'
             then return ""
             else do cs <- hGetLineUntilEOF h
                     return (c:cs)

-- This operation sends a results string over the handle with a list of requests.
-- If Nothing is used, all requests are send.
-- If Just is used, only the requests of the matching object type are send.
sendRequestNamesAndFormats :: Options -> Handle -> Maybe String -> IO ()
sendRequestNamesAndFormats opts handle mobj = case mobj of
  Just "package"   -> sendServerResult handle (unlines ("PACKAGES":listRequests packageConfiguration))
  Just "version"   -> sendServerResult handle (unlines ("VERSIONS":listRequests versionConfiguration))
  Just "module"    -> sendServerResult handle (unlines ("MODULES":listRequests moduleConfiguration))
  Just "type"      -> sendServerResult handle (unlines ("TYPES":listRequests typeConfiguration))
  Just "class"     -> sendServerResult handle (unlines ("CLASSES":listRequests classConfiguration))
  Just "operation" -> sendServerResult handle (unlines ("OPERATIONS":listRequests operationConfiguration))
  Just obj -> sendServerError opts handle $ "There are no requests for '" ++ obj ++ "'."
  Nothing -> sendServerResult handle $ unlines
    (  "PACKAGES":listRequests packageConfiguration
    ++ "\nVERSIONS":listRequests versionConfiguration
    ++ "\nMODULES":listRequests moduleConfiguration
    ++ "\nTYPES":listRequests typeConfiguration
    ++ "\nCLASSES":listRequests classConfiguration
    ++ "\nOPERATIONS":listRequests operationConfiguration
    )

-- This operation parses a string and returns a message for the server to process.
parseServerMessage :: String -> InfoServerMessage
parseServerMessage s = case words s of
  ["GetRequests"] -> GetRequests Nothing
  ["GetRequests", obj] -> GetRequests (Just obj)
  ["GetCommands"] -> GetCommands
  ["StopServer"] -> StopServer
  "RequestPackageInformation":outform:force:pkg:reqs -> RequestPackageInformation (readOutputFormat outform) (readForce force) pkg reqs
  "RequestVersionInformation":outform:force:pkg:vsn:reqs -> RequestVersionInformation (readOutputFormat outform) (readForce force) pkg vsn reqs
  "RequestModuleInformation":outform:force:pkg:vsn:m:reqs -> RequestModuleInformation (readOutputFormat outform) (readForce force) pkg vsn m reqs
  "RequestTypeInformation":outform:force:pkg:vsn:m:t:reqs -> RequestTypeInformation (readOutputFormat outform) (readForce force) pkg vsn m t reqs
  "RequestClassInformation":outform:force:pkg:vsn:m:c:reqs -> RequestClassInformation (readOutputFormat outform) (readForce force) pkg vsn m c reqs
  "RequestOperationInformation":outform:force:pkg:vsn:m:o:reqs -> RequestOperationInformation (readOutputFormat outform) (readForce force) pkg vsn m o reqs
  "RequestAllTypesInformation":outform:force:pkg:vsn:m:reqs -> RequestAllTypesInformation (readOutputFormat outform) (readForce force) pkg vsn m reqs
  "RequestAllClassesInformation":outform:force:pkg:vsn:m:reqs -> RequestAllClassesInformation (readOutputFormat outform) (readForce force) pkg vsn m reqs
  "RequestAllOperationsInformation":outform:force:pkg:vsn:m:reqs -> RequestAllOperationsInformation (readOutputFormat outform) (readForce force) pkg vsn m reqs
  _ -> ParseError

-- This operation tries to read an output format from a string.
readOutputFormat :: String -> Maybe OutFormat
readOutputFormat = safeRead

-- This operation tries to read a force value from a string.
readForce :: String -> Maybe Force
readForce = safeRead