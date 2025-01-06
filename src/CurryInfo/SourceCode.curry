------------------------------------------------------------------------------
--- This modules defines operations to find specific sections from
--- source code files.
------------------------------------------------------------------------------

module CurryInfo.SourceCode
  ( getSourceFilePath, SourceCode(..), readSourceCode, readDocumentation
  , getSourceCodeRef )
 where

import CurryInfo.Helper    ( quote )
import CurryInfo.Types
import CurryInfo.Checkout  ( checkoutIfMissing )
import CurryInfo.Commands  ( cmdCPMPath, runCmd, cmdCPMInstall
                           , getPackageLoadPath )
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage, printErrorMessage )

import System.CurryPath ( lookupModuleSource )
import System.Directory ( doesFileExist )
import System.FilePath  ( (</>), (<.>) )
import System.IO

import Data.List  ( isPrefixOf, isInfixOf, groupBy, last, elemIndex, findIndex )
import Data.Maybe ( fromMaybe )

type Checker = String -> Bool


-- This operation returns the line number of the contents of the given handle
-- (counted from the second argument)
-- in which the given definition (specified by the checker) starts.
getDefinitionLine :: Handle -> Checker -> Int -> IO (Maybe Int)
getDefinitionLine h check lnum = do
  eof <- hIsEOF h
  if eof then return Nothing
         else do l <- hGetLine h
                 if check l then return (Just lnum)
                            else getDefinitionLine h check (lnum+1)

-- This operation returns the lines of the contents of the given handle
-- up to the line where the predicate holds. This line is not included.
getLinesUpTo :: Handle -> Checker -> IO (Maybe [String])
getLinesUpTo = findLines []
 where
  findLines ls h check = do
    eof <- hIsEOF h
    if eof then return Nothing
           else do l <- hGetLine h
                   if check l then return (Just (reverse ls))
                              else findLines (l:ls) h check

-- This operation returns the line number of the contents of the given handle
-- (counted from the second argument) in which a definition of an entitity ends.
belongsLine :: Handle -> Checker -> Int -> IO Int
belongsLine h belong lnum = do
  eof <- hIsEOF h
  if eof then return lnum
         else do l <- hGetLine h
                 if belong l then belongsLine h belong (lnum+1)
                             else return lnum

-- This operation determines whether a line belongs to a definition.
belongs :: String -> Bool
belongs l = isPrefixOf " " l || isPrefixOf "\t" l || null l

--- This operation returns the directory and the actual path of the source file
--- of the given module.
getSourceFilePath :: Options -> Package -> Version -> Module
                  -> IO (Maybe (FilePath,FilePath))
getSourceFilePath opts pkg vsn m = do
  printDebugMessage opts $ "Getting source file path of " ++ quote m ++ "..."
  mpath <- checkoutIfMissing opts pkg vsn
  case mpath of
    Nothing -> do
      printDebugMessage opts "Package checkout failed: cannot get source file"
      return Nothing
    Just pkgpath -> do
      mlpath <- getPackageLoadPath opts pkgpath
      case mlpath of
        Nothing -> do printDebugMessage opts $ "Cannot determine load path " ++
                                               "for package '" ++ pkg ++ "'!"
                      return Nothing
        Just lpath -> do
          msrcpath <- lookupModuleSource lpath m
          case msrcpath of
            Nothing -> do printDebugMessage opts $ "Source file of module '" ++
                                                   m ++ "' not found!"
                          return Nothing
            Just dirpath -> return (Just dirpath)

--- This operation returns a handle to the source file of the given module
--- together with the path to the source file.
getSourceFileHandle :: Options -> Package -> Version -> Module
                    -> IO (Maybe (String, Handle))
getSourceFileHandle opts pkg vsn m = do
  printDebugMessage opts $ "Reading source file of module " ++ quote m ++ "..."
  mpath <- getSourceFilePath opts pkg vsn m
  case mpath of
    Nothing   -> return Nothing
    Just (_,path) -> do
      printDebugMessage opts $ "Path to source file: " ++ path
      b <- doesFileExist path
      case b of
        False -> do
          printDebugMessage opts $ "Source file does not exist: " ++ path
          return Nothing
        True -> do
          printDebugMessage opts $ "Opening source file " ++ quote path ++ "..."
          hdl <- openFile path ReadMode
          return (Just (path, hdl))

-- This operation looks for the part of the source code that corresponds to
-- the given checker and returns a reference to that part.
getSourceCodeRef :: Options -> Checker -> (String -> Bool) -> String -> Handle
                 -> IO (Maybe Reference)
getSourceCodeRef opts check belong path hdl = do
  printDebugMessage opts "Taking source code from file..."
  getDefinitionLine hdl check 0 >>= \mi -> case mi of
    Nothing -> do
      printDebugMessage opts "Could not find definition."
      hClose hdl
      return Nothing
    Just start -> do
      stop <- belongsLine hdl belong start
      hClose hdl
      return (Just (Reference path start stop))

-- This operation looks for the documentation that corresponds to the given
-- checker and returns a reference to that part.
getDocumentationRef :: Options -> Checker -> String -> Handle
                    -> IO (Maybe Reference)
getDocumentationRef opts check path hdl = do
  printDebugMessage opts "Taking documentation from file..."
  mls <- getLinesUpTo hdl check
  hClose hdl
  case mls of
    Nothing -> do
      printDebugMessage opts "Could not find definition."
      return Nothing
    Just ls -> do
      let rls  = dropWhile (all isSpace) (reverse ls)
          stop = length rls
      case takeWhile (isPrefixOf "--") rls of
        [] -> do printDebugMessage opts "Could not find documentation."
                 return Nothing
        gs -> return (Just (Reference path (stop - length gs) stop))

------------------------------------------------------------------------------

class SourceCode a where
  readSourceCode    :: Options -> a -> IO (Maybe Reference)
  readDocumentation :: Options -> a -> IO (Maybe Reference)

instance SourceCode CurryModule where
  readSourceCode opts (CurryModule pkg vsn m) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing -> return Nothing
      Just (path,h) -> do
        ls <- fmap lines (hGetContents h)
        let srclines = takeWhile (isPrefixOf "--") ls
        return (Just (Reference path (length srclines) (length ls)))
  
  readDocumentation opts (CurryModule pkg vsn m) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing       -> return Nothing
      Just (path,h) -> do
        mbdoc <- getLinesUpTo h (not . (isPrefixOf "--"))
        hClose h
        case mbdoc of
          Nothing  -> return Nothing
          Just doc -> return (Just (Reference path 0 (length doc)))

-- This operation returns a checker that looks for the definition
-- of the given type.
checkType :: Type -> Checker
checkType t l = checkTypeWords (words l)
 where
  checkTypeWords ws = case ws of
    x:y:zs -> (x == "external" && checkTypeWords (y:zs)) ||
              (x `elem` ["data", "type", "newtype"] &&
               t == takeWhile isAlphaNum y)
    _      -> False

instance SourceCode CurryType where
  readSourceCode opts (CurryType pkg vsn m t) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing       -> return Nothing
      Just (path,h) -> getSourceCodeRef opts (checkType t) belongs path h
  
  readDocumentation opts (CurryType pkg vsn m t) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing       -> return Nothing
      Just (path,h) -> getDocumentationRef opts (checkType t) path h

-- This operation returns a checker that look for the definition of the
-- given type class.
checkClass :: Class -> Checker
checkClass c l = let
    ls = words l
    classIndex = elemIndex "class" ls
    nameIndex = elemIndex c ls
    arrowIndex = elemIndex "=>" ls
  in
    if isPrefixOf "class" l && elem c ls
      then
        fromMaybe False ((<) <$> classIndex <*> nameIndex) &&
        fromMaybe True ((<) <$> arrowIndex <*> nameIndex)
      else False

instance SourceCode CurryClass where
  readSourceCode opts (CurryClass pkg vsn m c) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing       -> return Nothing
      Just (path,h) -> getSourceCodeRef opts (checkClass c) belongs path h
  
  readDocumentation opts (CurryClass pkg vsn m c) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing       -> return Nothing
      Just (path,h) -> getDocumentationRef opts (checkClass c) path h

-- This operation returns a checker that look for the definition of the
-- given operation.
checkOperation :: Operation -> Checker
checkOperation o l = let
    ls = words l
    operationIndex   = elemIndex o ls
    paranthesisIndex = elemIndex ("(" ++ o ++ ")") ls
    typingIndex      = elemIndex "::" ls
    equalIndex       = elemIndex "=" ls
    externalIndex    = elemIndex "external" ls
  in
    if (elem o ls || elem ("(" ++ o ++ ")") ls) &&
       (elem "::" ls || elem "=" ls || elem "external" ls)
      then fromMaybe False ((<) <$> operationIndex   <*> typingIndex  ) ||
           fromMaybe False ((<) <$> operationIndex   <*> equalIndex   ) ||
           fromMaybe False ((<) <$> operationIndex   <*> externalIndex) ||
           fromMaybe False ((<) <$> paranthesisIndex <*> typingIndex  ) ||
           fromMaybe False ((<) <$> paranthesisIndex <*> equalIndex   ) ||
           fromMaybe False ((<) <$> paranthesisIndex <*> externalIndex)
      else False

instance SourceCode CurryOperation where
  readSourceCode opts (CurryOperation pkg vsn m o) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing          -> return Nothing
      Just (path, hdl) -> do
        getSourceCodeRef opts (checkOperation o)
          (\l -> belongs l || checkOperation o l || isPrefixOf "#" l)
          path hdl
  
  readDocumentation opts (CurryOperation pkg vsn m o) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing          -> return Nothing
      Just (path, hdl) -> getDocumentationRef opts (checkOperation o) path hdl
