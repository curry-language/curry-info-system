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

data InfoServerMessage
    = GetRequests (Maybe String)
    | GetCommands
    | RequestPackageInformation (Maybe OutFormat) (Maybe Force) Package [String]
    | RequestVersionInformation (Maybe OutFormat) (Maybe Force) Package Version [String]
    | RequestModuleInformation (Maybe OutFormat) (Maybe Force) Package Version Module [String]
    | RequestTypeInformation (Maybe OutFormat) (Maybe Force) Package Version Module Type [String]
    | RequestTypeclassInformation (Maybe OutFormat) (Maybe Force) Package Version Module Typeclass [String]
    | RequestOperationInformation (Maybe OutFormat) (Maybe Force) Package Version Module Operation [String]
    | StopServer
    | ParseError

mainServer :: CConfig -> Maybe Int -> IO ()
mainServer cconfig mbport = do
    putStrLn "Start Server"
    (port1, socket1) <- maybe listenOnFresh
                        (\p -> listenOn p >>= \s -> return (p, s))
                        mbport
    putStrLn ("Server Port: " ++ show port1)
    storeServerPortNumber port1
    serverLoop cconfig socket1 []

serverLoop :: CConfig -> Socket -> [Handle] -> IO ()
serverLoop cconfig socket1 whandles = do
    connection <- waitForSocketAccept socket1 waitTime
    case connection of
        Just (_, handle) -> serverLoopOnHandle cconfig socket1 whandles handle
        Nothing -> do
            putStrLn "serverLoop: connection error: time out in waitForSocketAccept"
            sleep 1
            serverLoop cconfig socket1 whandles

serverLoopOnHandle :: CConfig -> Socket -> [Handle] -> Handle -> IO ()
serverLoopOnHandle cconfig socket1 whandles handle = do
    eof <- hIsEOF handle
    if eof
        then do
            hClose handle
            debugMessage dl 2 "SERVER connection: eof"
            serverLoop cconfig socket1 whandles
        else do
            string <- hGetLineUntilEOF handle
            debugMessage dl 2 ("SERVER got message: " ++ string)
            case parseServerMessage string of
                ParseError -> do
                    sendServerError dl handle ("Illegal message received: " ++ string)
                    serverLoopOnHandle cconfig socket1 whandles handle
                GetRequests mobj -> do
                    sendRequestNamesAndFormats dl handle mobj
                    serverLoopOnHandle cconfig socket1 whandles handle
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
                            , "StopServer"
                            ]
                    sendServerResult handle (unlines msg)
                    serverLoopOnHandle cconfig socket1 whandles handle
                RequestPackageInformation moutform mforce pkg reqs -> 
                    requestInformation moutform mforce [("packages", pkg)] reqs
                RequestVersionInformation moutform mforce pkg vsn reqs -> 
                    requestInformation moutform mforce [("packages", pkg), ("versions", vsn)] reqs
                RequestModuleInformation moutform mforce pkg vsn m reqs -> 
                    requestInformation moutform mforce [("packages", pkg), ("versions", vsn), ("modules", m)] reqs
                RequestTypeInformation moutform mforce pkg vsn m t reqs -> 
                    requestInformation moutform mforce [("packages", pkg), ("versions", vsn), ("modules", m), ("type", t)] reqs
                RequestTypeclassInformation moutform mforce pkg vsn m c reqs -> 
                    requestInformation moutform mforce [("packages", pkg), ("versions", vsn), ("modules", m), ("typeclass", c)] reqs
                RequestOperationInformation moutform mforce pkg vsn m o reqs -> 
                    requestInformation moutform mforce [("packages", pkg), ("versions", vsn), ("modules", m), ("operation", o)] reqs
                StopServer -> do
                    stopWorkers whandles
                    sendServerResult handle ""
                    hClose handle
                    close socket1
                    putStrLn "Stop Server"
                    removeServerPortNumber
                _ -> do
                    putStrLn "NOT IMPLEMENTED YET"
                    serverLoopOnHandle cconfig socket1 whandles handle
    where
        dl = debugLevel cconfig

        sendResult resultstring = do
            debugMessage dl 4 ("formatted result:\n" ++ resultstring)
            sendServerResult handle resultstring
            serverLoopOnHandle cconfig socket1 whandles handle
        
        sendRequestError err = do
            sendServerError dl handle ("Error in information server: " ++ show err)
            serverLoopOnHandle cconfig socket1 whandles handle
        
        requestInformation moutform mforce obj reqs = case (moutform, mforce) of
            (Nothing, Nothing) -> sendRequestError "Given output format and given force value do not exist"
            (Nothing, Just _) -> sendRequestError "Given output format does not exist"
            (Just _, Nothing) -> sendRequestError "Given force value does not exist"
            (Just outform, Just force) -> catch
                (getInfos (silentOptions {optForce = force, optOutput = outform}) obj reqs >>= printResult >>= sendResult)
                sendRequestError

sendServerResult :: Handle -> String -> IO ()
sendServerResult handle resultstring = do
    let resultlines = lines resultstring
    hPutStrLn handle ("ok " ++ show (length resultlines))
    hPutStrLn handle (unlines resultlines)
    hFlush handle

sendServerError :: DLevel -> Handle -> String -> IO ()
sendServerError dl handle errstring = do
    debugMessage dl 1 errstring
    hPutStrLn handle ("error " ++ errstring)
    hFlush handle

hGetLineUntilEOF  :: Handle -> IO String
hGetLineUntilEOF h = do
  eof <- hIsEOF h
  if eof
   then return ""
   else do c <- hGetChar h
           if c=='\n' then return ""
                      else do cs <- hGetLineUntilEOF h
                              return (c:cs)

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

stopWorkers :: [Handle] -> IO ()
stopWorkers _ = return ()

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
    _ -> ParseError

readOutputFormat :: String -> Maybe OutFormat
readOutputFormat = safeRead

readForce :: String -> Maybe Force
readForce = safeRead