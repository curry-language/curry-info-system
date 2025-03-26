------------------------------------------------------------------------------
--- This module implements a transformation from the CurryInfo cache
--- into an HTML representation.
---
--- @author Michael Hanus
--- @version March 2025
------------------------------------------------------------------------------

module CurryInfo.ToHTML ( generateCurryInfoHTML )
 where

import Control.Monad       ( unless, when )
import Data.Char           ( toLower )
import Data.Either         ( partitionEithers )
import Data.List           ( intercalate, isPrefixOf, isSuffixOf, last, sort )

import Data.Time           ( CalendarTime, calendarTimeToString, getLocalTime )
import HTML.Base
import HTML.Styles.Bootstrap4
import JSON.Pretty              ( ppJSON )
import Language.Curry.Resources ( cpmHomeURL, curryHomeURL, masalaHomeURL, curryPackagesURL )
import System.Directory         ( createDirectory, doesDirectoryExist
                                , doesFileExist, getDirectoryContents
                                , getAbsolutePath )
import System.FilePath          ( (</>), (<.>) )
import System.Process           ( exitWith, system )

import CurryInfo.ConfigPackage  ( getPackagePath )
import CurryInfo.Helper         ( isCurryID, quote )
import CurryInfo.Information    ( getInfos, printResult )
import CurryInfo.Options        ( getDefaultOptions, getDefaultOptions )
import CurryInfo.Paths          ( jsonFile2Name, encodeFilePath, packagesPath )
import CurryInfo.Types          ( Options(..), QueryObject(..), prettyObject
                                , OutFormat(..), Output(..) )
import CurryInfo.Verbosity      ( printStatusMessage, printErrorMessage )

------------------------------------------------------------------------------
--- Generate HTML pages for the CurryInfo cache.
generateCurryInfoHTML :: Options -> IO ()
generateCurryInfoHTML opts = do
  htmldir    <- getAbsolutePath (optHTMLDir opts)
  when (optForce opts > 1) $
    (system ("/bin/rm -rf " ++ quote htmldir) >> return ())
  exhtmldir  <- doesDirectoryExist htmldir
  exhtmlfile <- doesFileExist htmldir
  when (exhtmldir || exhtmlfile) $ do
    printErrorMessage $ "'" ++ htmldir ++ "' exists!"
    printErrorMessage
      "Use '-f2' to  delete it or select another target directory!"
    exitWith 1
  printStatusMessage opts $
    "Copying CurryInfo cache to " ++ quote htmldir ++ "..."
  createDirectory htmldir
  system $ "/bin/cp -a " ++ quote (packagesPath opts) ++ " " ++ quote htmldir
  printStatusMessage opts $ "Creating HTML files in " ++ quote htmldir ++ "..."
  directoryAsHTML opts ("index.html", [htxt $ "CurryInfo: All Packages"])
                  1 htmldir ["packages"]
  pipath <- (</> "include") <$> getPackagePath
  ec <- copyIncludes pipath htmldir
  unless (ec == 0) $ (copyIncludes "include" htmldir >> return ())
  return ()
 where
  copyIncludes idir htmldir = do
    exidir <- doesDirectoryExist idir
    if exidir
      then do
        system $ "/bin/cp -a " ++ quote (idir </> "bt4") ++ " " ++ quote htmldir
        system $
          "/bin/cp " ++ quote (idir </> "index.html") ++ " " ++ quote htmldir
        system $ "chmod -R go+rX " ++ quote htmldir
      else return 1

------------------------------------------------------------------------------
-- Generates HTML representation of the contents of a directory of the
-- CurryInfo cache.
directoryAsHTML :: Options -> (String,[BaseHtml]) -> Int -> FilePath -> [String]
                -> IO ()
directoryAsHTML opts (home,brand) d base dirs = do
  exdir <- doesDirectoryExist basedir
  when exdir $ do
    let (mbobj,cmt) = dirs2Object dirs
        navobj      = maybe "" prettyObject mbobj ++ cmt
    direlems <- sort <$> getRealFilesInDir base dirs
    (dirfiles,subdirs) <- partitionEithers <$>
                            mapM returnEitherFileOrDir direlems
    mapM_ (\sd -> directoryAsHTML opts ("../" ++ home, brand) (d+1)
                                  base (dirs ++ [sd])) subdirs
    hfiles <- dirElems2HTML cmt dirfiles
    hdirs  <- dirElems2HTML cmt subdirs
    htmldoc <- subdirHtmlPage navobj (home,brand) d navobj (hfiles ++ hdirs)
    let htmlsrcfile = basedir </> "index.html"
    writeFile htmlsrcfile htmldoc
    printStatusMessage opts $ htmlsrcfile ++ " written"
 where
  basedir = foldr1 (</>) (base:dirs)

  returnEitherFileOrDir f = do
    exdir <- doesDirectoryExist (basedir </> f)
    return $ if exdir then Right f else Left f

  -- shows references to a list of directory elements either as an HTML list
  -- or a paragraph of words, if the list contains more than 10 elements
  dirElems2HTML cmt ds = do
    hrefs <- filter (not . null) <$> mapM (fileAsHTML cmt) ds
    return $ if null hrefs
               then []
               else if length hrefs <= 10
                      then [ulist hrefs `addAttr` ("style","list-style: none")]
                      else [par (intercalate [nbsp] hrefs)]

  fileAsHTML cmt filename =
    maybe (do nfs <- length <$> getRealFilesInDir base (dirs ++ [filename])
              return $ if nfs == 0
                         then []
                         else [toHtml hrefScndBadge filename filename])
          (\n -> do fn <- generateFromJSON n
                    return [toHtml hrefPrimBadge fn n])
          (jsonFile2Name filename)
   where
    toHtml hrefkind ref name = code [hrefkind ref [htxt name]]

    generateFromJSON n
      | any (`isSuffixOf` cmt) ["operations", "types", "classes"]
      = case dirs2Object (dirs ++ [n]) of
          (Just obj,_) -> genInfoResult obj
          _            -> return filename
      | null cmt
      = case dirs2Object dirs of
          (Just obj,_) -> genInfoResult obj
          _            -> return filename
      | otherwise
      = return filename
     where
      genInfoResult obj = do
        printStatusMessage opts $
          "Generating infos for object: " ++ prettyObject obj
        let htmlopts = opts { optShowAll = True, optOutFormat = OutText
                            , optColor = True }
        infotxt <- resultText <$> getInfos htmlopts obj []
        let navobj = prettyObject obj
        docs <- subdirHtmlPage navobj (home,brand) d navobj
                               (structureText infotxt)
        let htmlfile = encodeFilePath n <.> "html"
        writeFile (basedir </> htmlfile) docs
        return htmlfile

-- Returns the "real" entity files in a directory (represented as a list).
-- The first argument is the base directory preprended to the directory files.
getRealFilesInDir :: String -> [String] -> IO [String]
getRealFilesInDir base dirs = do
  let dirname = foldr1 (</>) (base:dirs)
  exdir <- doesDirectoryExist dirname
  if exdir then filter isReal <$> getDirectoryContents dirname
           else return []
 where
  isOperation = last dirs == "operations"

  isReal fn = not ("." `isPrefixOf` fn) &&
              not (".html" `isSuffixOf` fn) &&
              not (".txt" `isSuffixOf` fn) &&
              maybe True
                    (\n -> n /= "_DUMMY_" && (not isOperation || isCurryID n))
                    (jsonFile2Name fn)

-- Use the coloring of CurryInfo results to structure the result text.
structureText :: HTML a => String -> [a]
structureText t =
  let (t1,rest) = upToEsc32 t
      start   = strip t1
      rows = (if null start then [] else [[[nbsp], [verbatim start]]]) ++
             if null rest then [] else text2table rest
  in  [table rows `addClass` "table table-striped table-sm"]
 where
 text2table s = -- the chars before `s` are "\ESC[32m"
   let (colt,ds) = upToStop s
       (cnt,es)  = upToEsc32 ds
   in [[bold [htxt $ strip colt]], [verbatim $ stripCR cnt]] :
      if null es then [] else text2table es

 upToEsc32 xs = case xs of '\ESC':'[':'3':'2':'m':ys -> ([],ys)
                           []   -> ([],[])
                           c:cs -> let (ys,zs) = upToEsc32 cs in (c:ys,zs)

 upToStop xs = case xs of '\ESC':'[':'0':'m':ys -> ([],ys)
                          []   -> ([],[])
                          c:cs -> let (ys,zs) = upToStop cs in (c:ys,zs)

 strip = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')
 stripCR = reverse . dropWhile (=='\n') . reverse . dropWhile (=='\n')

resultText :: Output -> String
resultText (OutputText txt)  = txt
resultText (OutputJSON jv)   = ppJSON jv
resultText (OutputTerm ts)   = show ts
resultText (OutputError err) = "Error: " ++ err


-- Generates a HTML page in a subdirectory of CurryInfo with a given page title,
-- home brand, header and contents.
subdirHtmlPage :: String -> (String,[BaseHtml]) -> Int -> String -> [BaseHtml]
               -> IO String
subdirHtmlPage pagetitle homebrand d headerinfo maindoc = do
  time <- getLocalTime
  let btbase = concat (take d (repeat "../")) ++ "bt4"
      header = [h1 [htxt "CurryInfo: ", smallMutedText headerinfo]]
  return $ showHtmlPage $
    bootstrapPage (favIcon btbase) (cssIncludes btbase) (jsIncludes btbase)
      pagetitle homebrand leftTopMenu rightTopMenu 0 [] header
      maindoc (curryDocFooter time)


-- Maps a path as used in the CurryInfo cache into a `QueryObject` and
-- a possible title suffix for the generated HTML page.
dirs2Object :: [String] -> (Maybe QueryObject, String)
dirs2Object dirs = case dirs of
  ["packages"]      -> (Nothing, "All packages")
  ["packages", pkg] -> (Just (QueryPackage pkg), "")
  ["packages", pkg, "versions"] -> (Just (QueryPackage pkg), ": all versions")
  ["packages", pkg, "versions", vsn] -> (Just (QueryVersion pkg vsn), "")
  ["packages", pkg, "versions", vsn, "modules"] ->
    (Just (QueryVersion pkg vsn), ": all modules")
  ["packages", pkg, "versions", vsn, "modules", m] ->
    (Just (QueryModule pkg vsn m), "")
  ["packages", pkg, "versions", vsn, "modules", m, "types"] ->
    (Just (QueryModule pkg vsn m), ": all types")
  ["packages", pkg, "versions", vsn, "modules", m, "types", t] ->
    (Just (QueryType pkg vsn m t), "")
  ["packages", pkg, "versions", vsn, "modules", m, "classes"] ->
    (Just (QueryModule pkg vsn m), ": all classes")
  ["packages", pkg, "versions", vsn, "modules", m, "classes", c] ->
    (Just (QueryClass pkg vsn m c), "")
  ["packages", pkg, "versions", vsn, "modules", m, "operations"] ->
    (Just (QueryModule pkg vsn m), ": all operations")
  ["packages", pkg, "versions", vsn, "modules", m, "operations", o] ->
    (Just (QueryOperation pkg vsn m o), "")
  _ -> (Nothing, "")

------------------------------------------------------------------------------
-- Helpers:

--- A small muted text (used in the title):
smallMutedText :: String -> BaseHtml
smallMutedText s = htmlStruct "small" [("class","text-muted")] [htxt s]

-- The URL of the favicon relative to the base directory of BT4.
favIcon :: String -> String
favIcon btdir = btdir </> "img" </> "favicon.ico"

-- The CSS includes relative to the base directory of BT4.
cssIncludes :: String -> [String]
cssIncludes btdir =
  map (\n -> btdir </> "css" </> n ++ ".css") ["bootstrap.min", "cpm"]

-- The JavaScript includes relative to the base directory of BT4.
jsIncludes :: String -> [String]
jsIncludes btdir =
   ["https://code.jquery.com/jquery-3.4.1.slim.min.js",
    btdir </> "js/bootstrap.bundle.min.js"]

--- The standard left top menu.
--- The first argument is true if we are inside a package documentation.
--- The second argument indicates the index of the active link
--- (negative value = no active link)
leftTopMenu :: [[BaseHtml]]
leftTopMenu =
  [ [ehrefNav "https://github.com/curry-language/curry-info-system/blob/main/README.md"
              [htxt "About CurryInfo"]]
  , [ehrefNav curryPackagesURL [htxt "CPM Repository"]]
  ]

--- The standard right top menu.
rightTopMenu :: [[BaseHtml]]
rightTopMenu =
  [ [ehrefNav masalaHomeURL [htxt "Masala"]]
  , [ehrefNav cpmHomeURL    [htxt "Curry Package Manager"]]
  , [ehrefNav curryHomeURL  [htxt "Curry Homepage"]]
  ]

-- Standard footer information for generated web pages:
curryDocFooter :: CalendarTime -> [BaseHtml]
curryDocFooter time =
  [italic [htxt "Generated by CurryInfo at ",
           htxt (calendarTimeToString time)]]

------------------------------------------------------------------------------
