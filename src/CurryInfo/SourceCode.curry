------------------------------------------------------------------------------
--- This modules defines operations to find specific sections from
--- source code files.
------------------------------------------------------------------------------

module CurryInfo.SourceCode
  ( getSourceFilePath, readModuleDocumentation, readModuleSourceCode
  , SourceCode(..) )
 where

import Control.Monad      ( unless )
import Data.List          ( intercalate, isPrefixOf )
import System.Environment ( setEnv )
import System.IO

import JSON.Convert     ( toJSON )
import Language.Curry.SourceCodeClassifier
import System.CurryPath ( lookupModuleSource )
import System.Directory ( doesFileExist )
import System.FilePath  ( searchPathSeparator )

import CurryInfo.Helper    ( quote )
import CurryInfo.JConvert
import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Checkout  ( checkoutIfMissing )
import CurryInfo.Commands  ( cmdCPMPath, runCmd, getPackageLoadPath )
import CurryInfo.Paths     ( stripRootPath )
import CurryInfo.Verbosity ( printStatusMessage, printDetailMessage
                           , printDebugMessage, printErrorMessage )
import CurryInfo.Writer    ( updateObjectInformation )

------------------------------------------------------------------------------

--- This operation returns the package load path, the directory,
--- and the actual path of the source file of the given module of the package.
getSourceFilePath :: Options -> Package -> Version -> Module
                  -> IO (Maybe ([String],FilePath,FilePath))
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
          let cpath = intercalate [searchPathSeparator] lpath
          printDebugMessage opts $ "Load path of package: " ++ cpath
          unless (null lpath) $ setEnv "CURRYPATH" cpath
          msrcpath <- lookupModuleSource lpath m
          case msrcpath of
            Nothing -> do printDebugMessage opts $ "Source file of module '" ++
                                                   m ++ "' not found!"
                          return Nothing
            Just (dir,path) -> return (Just (lpath,dir,path))

--- This operation returns a handle to the source file of the given module
--- together with the path to the source file.
getSourceFileHandle :: Options -> Package -> Version -> Module
                    -> IO (Maybe (String, Handle))
getSourceFileHandle opts pkg vsn m = do
  printDebugMessage opts $ "Reading source file of module " ++ quote m ++ "..."
  mpath <- getSourceFilePath opts pkg vsn m
  case mpath of
    Nothing   -> return Nothing
    Just (_,_,path) -> do
      printDebugMessage opts $ "Path to source file: " ++ path
      b <- doesFileExist path
      case b of
        False -> do
          printDebugMessage opts $ "Source file does not exist: " ++ path
          return Nothing
        True -> do
          printDebugMessage opts $ "Opening source file " ++ quote path ++ "..."
          hdl <- openFile path ReadMode
          return (Just (stripRootPath opts path, hdl))

-- This operation returns the lines of the contents of the given handle
-- up to the line where the predicate holds. This line is not included.
getLinesUpTo :: Handle -> (String -> Bool) -> IO (Maybe [String])
getLinesUpTo = findLines []
 where
  findLines ls h check = do
    eof <- hIsEOF h
    if eof then return Nothing
           else do l <- hGetLine h
                   if check l then return (Just (reverse ls))
                              else findLines (l:ls) h check

------------------------------------------------------------------------------
--- Get reference of the module source code.
readModuleSourceCode :: Options -> CurryModule -> IO (Maybe Reference)
readModuleSourceCode opts (CurryModule pkg vsn m) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing -> return Nothing
      Just (path,h) -> do
        ls <- fmap lines (hGetContents h)
        let srclines = takeWhile (isPrefixOf "--") ls
        return (Just (Reference path (length srclines) (length ls)))

--- Get reference of the module documentation.
readModuleDocumentation :: Options -> CurryModule -> IO (Maybe Reference)
readModuleDocumentation opts (CurryModule pkg vsn m) = do
    mresult <- getSourceFileHandle opts pkg vsn m
    case mresult of
      Nothing       -> return Nothing
      Just (path,h) -> do
        mbdoc <- getLinesUpTo h (not . (isPrefixOf "--"))
        hClose h
        case mbdoc of
          Nothing  -> return Nothing
          Just doc -> return (Just (Reference path 0 (length doc)))

------------------------------------------------------------------------------
-- Operations to read the entities of a module.

-- Type class for module entities supporting reading the documentation
-- and source code of entities. The second argument of the methods
-- are the possible list of exported entities.
class SourceCode a where
  readSourceCode    :: Options -> Maybe [String] -> a -> IO (Maybe Reference)
  readDocumentation :: Options -> Maybe [String] -> a -> IO (Maybe Reference)

instance SourceCode CurryType where
  readSourceCode opts exports (CurryType pkg vsn mn en) = do
    srcrefs <- readSourceReferences opts pkg vsn mn
                 (map (\(t,_,r) -> (t,r)) <$> getTypesInModule mn)
                 (QueryType pkg vsn mn) "definition" exports
    returnEntityRef en srcrefs

  readDocumentation opts exports (CurryType pkg vsn mn en) = do
    docrefs <- readSourceReferences opts pkg vsn mn
                 (map (\(t,r,_) -> (t,r)) <$> getTypesInModule mn)
                 (QueryType pkg vsn mn) "documentation" exports
    returnEntityRef en docrefs

instance SourceCode CurryClass where
  readSourceCode opts exports (CurryClass pkg vsn mn en) = do
    srcrefs <- readSourceReferences opts pkg vsn mn
                 (map (\(c,_,r) -> (c,r)) <$> getClassesInModule mn)
                 (QueryClass pkg vsn mn) "definition" exports
    returnEntityRef en srcrefs

  readDocumentation opts exports (CurryClass pkg vsn mn en) = do
    docrefs <- readSourceReferences opts pkg vsn mn
                 (map (\(c,r,_) -> (c,r)) <$> getClassesInModule mn)
                 (QueryClass pkg vsn mn) "documentation" exports
    returnEntityRef en docrefs

instance SourceCode CurryOperation where
  readSourceCode opts exports (CurryOperation pkg vsn mn op) = do
    oprefs <- readSourceReferences opts pkg vsn mn
                (map (\(o,_,r) -> (o,r)) <$> getOperationsInModule mn)
                (QueryOperation pkg vsn mn) "definition" exports
    returnEntityRef op oprefs

  readDocumentation opts exports (CurryOperation pkg vsn mn op) = do
    oprefs <- readSourceReferences opts pkg vsn mn
                (map (\(o,r,_) -> (o,r)) <$> getOperationsInModule mn)
                (QueryOperation pkg vsn mn) "documentation" exports
    returnEntityRef op oprefs

returnEntityRef :: String -> Maybe [(String,Reference)] -> IO (Maybe Reference)
returnEntityRef ent refs = do
  return $ if null ent then Just (Reference "" 0 0)
                       else maybe Nothing (lookup ent) refs

-- Generic operation which reads source code references of entities
-- in a module and stores them.
-- The parameters are the options, package, version, module,
-- an IO action which reads all source code references,
-- a `QueryObject` constructor for entities, the field/request name,
-- and a possible list of exported entity names.
-- The list of exported entities together with their references is returned.
readSourceReferences :: Options -> Package -> Version -> Module
                     -> IO [(String,(Int,Int))] -> (String -> QueryObject)
                     -> String -> Maybe [String]
                     -> IO (Maybe [(String,Reference)])
readSourceReferences opts pkg vsn mn readrefs qoconstr field mbexports = do
  mpath <- getSourceFilePath opts pkg vsn mn
  case mpath of
    Nothing   -> return Nothing
    Just (loadpath,_,path) -> do
      unless (null loadpath) $
        setEnv "CURRYPATH" (intercalate [searchPathSeparator] loadpath)
      -- ignore entities with empty reference:
      ents <- filter (\(_,(from,to)) -> to > from) <$> readrefs
      let expents = maybe ents
                          (\exports -> filter ((`elem` exports) . fst) ents)
                          mbexports
      printDebugMessage opts $
        "Source code lines in module " ++ quote mn ++ "\n" ++ show expents
      let spath = stripRootPath opts path
      entrefs <- mapM (\(e,(from,to)) ->
                          addReference e (Reference spath (from-1) (to-1)))
                      expents
      return (Just entrefs)
 where
  addReference n r = do
    updateObjectInformation opts (qoconstr n) [(field, toJSON r)]
    return (n, r)
