-----------------------------------------------------------------------------------------
--- This modules defines operations to run the tool in server mode.
-----------------------------------------------------------------------------------------

module CurryInfo.Server.Server where

import CurryInfo.Types
import CurryInfo.Options
import CurryInfo.Information (getInfos, printResult)
import CurryInfo.Configuration
import CurryInfo.Helper (safeRead)
import CurryInfo.Server.Configuration

import System.Process (system, sleep)
import System.IO (Handle, hIsEOF, hClose, hFlush, hPutStrLn, hGetChar)

import Network.Socket (Socket(..), listenOn, listenOnFresh
            , close, waitForSocketAccept)

type Force = Int

data SingleOrAll = Single | AllTypes | AllTypeclasses | AllOperations

data InfoServerMessage
  = GetRequests (Maybe String)
  | GetCommands
  | RequestPackageInformation (Maybe OutFormat) (Maybe Force) Package [String]
  | RequestVersionInformation (Maybe OutFormat) (Maybe Force) Package Version [String]
  | RequestModuleInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | RequestTypeInformation (Maybe OutFormat) (Maybe Force) Package Version Module Type [String]
  | RequestTypeclassInformation (Maybe OutFormat) (Maybe Force) Package Version Module Typeclass [String]
  | RequestOperationInformation (Maybe OutFormat) (Maybe Force) Package Version Module Operation [String]
  | RequestAllTypesInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | RequestAllTypeclassesInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | RequestAllOperationsInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
  | StopServer
  | ParseError

-- This action starts the server.
mainServer :: CConfig -> Maybe Int -> IO ()
mainServer cconfig mbport = do
  putStrLn "Start Server"
  (port1, socket1) <- maybe listenOnFresh
            (\p -> listenOn p >>= \s -> return (p, s))
            mbport
  putStrLn ("Server Port: " ++ show port1)
  storeServerPortNumber port1
  serverLoop cconfig socket1

-- This action is the main server loop.
serverLoop :: CConfig -> Socket -> IO ()
serverLoop cconfig socket1 = do
  connection <- waitForSocketAccept socket1 waitTime
  case connection of
    Just (_, handle) -> serverLoopOnHandle cconfig socket1 handle
    Nothing -> do
      putStrLn "serverLoop: connection error: time out in waitForSocketAccept"
      sleep 1
      serverLoop cconfig socket1

-- This action is the main loop of handling communication after successfully connecting.
serverLoopOnHandle :: CConfig -> Socket -> Handle -> IO ()
serverLoopOnHandle cconfig socket1 handle = do
  eof <- hIsEOF handle
  if eof
    then do
      hClose handle
      debugMessage dl 2 "SERVER connection: eof"
      serverLoop cconfig socket1
    else do
      string <- hGetLineUntilEOF handle
      debugMessage dl 2 ("SERVER got message: " ++ string)
      case parseServerMessage string of
        ParseError -> do
          sendServerError dl handle ("Illegal message received: " ++ string)
          serverLoopOnHandle cconfig socket1 handle
        GetRequests mobj -> do
          sendRequestNamesAndFormats dl handle mobj
          serverLoopOnHandle cconfig socket1 handle
        GetCommands -> do
          let msg =
                [ "GetRequests | GetRequests <obj>"
                , "GetCommands"
                , "RequestPackageInformation <outform> <force> <pkg> <reqs>"
                , "RequestVersionInformation <outform> <force> <pkg> <vsn> <reqs>"
                , "RequestModuleInformation <outform> <force> <pkg> <vsn> <mod>  <reqs>"
                , "RequestTypeInformation <outform> <force> <pkg> <vsn> <mod> <t> <reqs>"
                , "RequestTypeclassInformation <outform> <force> <pkg> <vsn> <mod> <tc> <reqs>"
                , "RequestOperationInformation <outform> <force> <pkg> <vsn> <mod> <op> <reqs>"
                , "RequestAllTypesInformation <outform> <force> <pkg> <vsn> <mod> <reqs"
                , "RequestAllTypeclassesInformation <outform> <force> <pkg> <vsn> <mod> <reqs"
                , "RequestAllOperationsInformation <outform> <force> <pkg> <vsn> <mod> <reqs"
                , "StopServer"
                ]
          sendServerResult handle (unlines msg)
          serverLoopOnHandle cconfig socket1 handle
        RequestPackageInformation moutform mforce pkg reqs -> 
          requestInformation moutform mforce Single [("packages", pkg)] reqs
        RequestVersionInformation moutform mforce pkg vsn reqs -> 
          requestInformation moutform mforce Single [("packages", pkg), ("versions", vsn)] reqs
        RequestModuleInformation moutform mforce pkg vsn m reqs -> 
          requestInformation moutform mforce Single [("packages", pkg), ("versions", vsn), ("modules", m)] reqs
        RequestTypeInformation moutform mforce pkg vsn m t reqs -> 
          requestInformation moutform mforce Single [("packages", pkg), ("versions", vsn), ("modules", m), ("types", t)] reqs
        RequestTypeclassInformation moutform mforce pkg vsn m c reqs -> 
          requestInformation moutform mforce Single [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclasses", c)] reqs
        RequestOperationInformation moutform mforce pkg vsn m o reqs -> 
          requestInformation moutform mforce Single [("packages", pkg), ("versions", vsn), ("modules", m), ("operations", o)] reqs
        RequestAllTypesInformation moutform mforce pkg vsn m reqs ->
          requestInformation moutform mforce AllTypes [("packages", pkg), ("versions", vsn), ("modules", m)] reqs
        RequestAllTypeclassesInformation moutform mforce pkg vsn m reqs ->
          requestInformation moutform mforce AllTypeclasses [("packages", pkg), ("versions", vsn), ("modules", m)] reqs
        RequestAllOperationsInformation moutform mforce pkg vsn m reqs ->
          requestInformation moutform mforce AllOperations [("packages", pkg), ("versions", vsn), ("modules", m)] reqs
        StopServer -> do
          sendServerResult handle ""
          hClose handle
          close socket1
          putStrLn "Stop Server"
          removeServerPortNumber
        -- _ -> do putStrLn "NOT IMPLEMENTED YET"
        --         serverLoopOnHandle cconfig socket1 handle
  where
    dl = debugLevel cconfig

    sendResult resultstring = do
      debugMessage dl 4 ("formatted result:\n" ++ resultstring)
      sendServerResult handle resultstring
      serverLoopOnHandle cconfig socket1 handle
    
    sendRequestError err = do
      sendServerError dl handle ("Error in information server: " ++ show err)
      serverLoopOnHandle cconfig socket1 handle
    
    requestInformation moutform mforce singleOrAll obj reqs = case (moutform, mforce) of
      (Nothing, Nothing) -> sendRequestError "Given output format and given force value do not exist"
      (Nothing, Just _) -> sendRequestError "Given output format does not exist"
      (Just _, Nothing) -> sendRequestError "Given force value does not exist"
      (Just outform, Just force) -> catch
        (getInfos (change singleOrAll (silentOptions {optForce = force, optOutput = outform})) obj reqs >>= printResult >>= sendResult)
        sendRequestError
    
    change singleOrAll opts = case singleOrAll of
      Single -> opts
      AllTypes -> opts {optAllTypes = True}
      AllTypeclasses -> opts {optAllTypeclasses = True}
      AllOperations -> opts {optAllOperations = True}

-- This action sends a result string over the given handle.
sendServerResult :: Handle -> String -> IO ()
sendServerResult handle resultstring = do
  let resultlines = lines resultstring
  hPutStrLn handle ("ok " ++ show (length resultlines))
  hPutStrLn handle (unlines resultlines)
  hFlush handle

-- This action sends an error string over the given handle.
sendServerError :: DLevel -> Handle -> String -> IO ()
sendServerError dl handle errstring = do
  debugMessage dl 1 errstring
  hPutStrLn handle ("error " ++ errstring)
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
sendRequestNamesAndFormats :: DLevel -> Handle -> Maybe String -> IO ()
sendRequestNamesAndFormats dl handle mobj = case mobj of
  Just "package" -> sendServerResult handle (unlines ("PACKAGES":listRequests packageConfiguration))
  Just "version" -> sendServerResult handle (unlines ("VERSIONS":listRequests versionConfiguration))
  Just "module" -> sendServerResult handle (unlines ("MODULES":listRequests moduleConfiguration))
  Just "type" -> sendServerResult handle (unlines ("TYPES":listRequests typeConfiguration))
  Just "typeclass" -> sendServerResult handle (unlines ("TYPECLASSES":listRequests typeclassConfiguration))
  Just "operation" -> sendServerResult handle (unlines ("OPERATIONS":listRequests operationConfiguration))
  Just obj -> sendServerError dl handle $ "There are no requests for '" ++ obj ++ "'."
  Nothing -> sendServerResult handle $ unlines
    (  "PACKAGES":listRequests packageConfiguration
    ++ "\nVERSIONS":listRequests versionConfiguration
    ++ "\nMODULES":listRequests moduleConfiguration
    ++ "\nTYPES":listRequests typeConfiguration
    ++ "\nTYPECLASSES":listRequests typeclassConfiguration
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
  "RequestTypeclassInformation":outform:force:pkg:vsn:m:c:reqs -> RequestTypeclassInformation (readOutputFormat outform) (readForce force) pkg vsn m c reqs
  "RequestOperationInformation":outform:force:pkg:vsn:m:o:reqs -> RequestOperationInformation (readOutputFormat outform) (readForce force) pkg vsn m o reqs
  "RequestAllTypesInformation":outform:force:pkg:vsn:m:reqs -> RequestAllTypesInformation (readOutputFormat outform) (readForce force) pkg vsn m reqs
  "RequestAllTypeclassesInformation":outform:force:pkg:vsn:m:reqs -> RequestAllTypeclassesInformation (readOutputFormat outform) (readForce force) pkg vsn m reqs
  "RequestAllOperationsInformation":outform:force:pkg:vsn:m:reqs -> RequestAllOperationsInformation (readOutputFormat outform) (readForce force) pkg vsn m reqs
  _ -> ParseError

-- This operation tries to read an output format from a string.
readOutputFormat :: String -> Maybe OutFormat
readOutputFormat = safeRead

-- This operation tries to read a force value from a string.
readForce :: String -> Maybe Force
readForce = safeRead