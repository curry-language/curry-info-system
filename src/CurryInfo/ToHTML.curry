------------------------------------------------------------------------------
--- This module implements a transformation from the CurryInfo cache
--- into an HTML representation.
---
--- @author Michael Hanus
--- @version March 2025
------------------------------------------------------------------------------

module CurryInfo.ToHTML
 where

import Control.Monad       ( unless, when )
import Data.Char           ( toLower )
import Data.List           ( intercalate, isPrefixOf, isSuffixOf, last, sort )

import Data.Time           ( CalendarTime, calendarTimeToString, getLocalTime )
import HTML.Base
import HTML.Styles.Bootstrap4
import JSON.Pretty         ( ppJSON )
import Language.Curry.Resources ( cpmHomeURL, curryHomeURL )
import System.Directory    ( doesDirectoryExist, getDirectoryContents
                           , doesFileExist )
import System.FilePath     ( (</>), (<.>) )
import System.Process      ( exitWith, system )

import CurryInfo.Helper      ( isCurryID )
import CurryInfo.Information ( getInfos, printResult )
import CurryInfo.Options     ( getDefaultOptions, getDefaultOptions )
import CurryInfo.Paths       ( jsonFile2Name, encodeFilePath )
import CurryInfo.Types       ( Options(..), QueryObject(..), prettyObject
                             , OutFormat(..), Output(..), optCacheRoot, optHTMLDir )

------------------------------------------------------------------------------
--- Generate HTML pages for the CurryInfo cache.
generateCurryInfoHTML :: Options -> IO ()
generateCurryInfoHTML opts = do
  let basedir = optHTMLDir opts
  exbasedir  <- doesDirectoryExist basedir
  exbasefile <- doesFileExist basedir
  when (exbasedir || exbasefile) $ do
    putStrLn $ "'" ++ basedir ++ "' exists!"
    putStrLn "Please delete it or select another target directory!"
    exitWith 1
  putStrLn $ "Copying CurryInfo cache to '" ++ basedir ++ "'..."
  system $ "/bin/cp -a '" ++ optCacheRoot opts ++ "' '" ++ basedir ++ "'"
  directoryAsHTML opts ("index.html", [htxt $ "All Packages"])
                  1 basedir ["packages"]

------------------------------------------------------------------------------
-- Generates a HTML representation of the contents of a directory.
directoryAsHTML :: Options -> (String,[BaseHtml]) -> Int -> FilePath -> [String] -> IO ()
directoryAsHTML opts (home,brand) d base dirs = do
  exdir <- doesDirectoryExist basedir
  when exdir $ do
    let (mbobj,cmt) = dirs2Object dirs
        navobj = maybe "" prettyObject mbobj ++ cmt
    dirfiles <- filter isReal <$> getDirectoryContents basedir
    doc <- if null dirfiles
             then return []
             else do
               hfiles <- mapM (fileAsHTML cmt) (sort dirfiles)
               subdirs <- concat <$> mapM returnExDir dirfiles
               mapM_ (\sd -> directoryAsHTML opts ("../" ++ home, brand) (d+1)
                                             base (dirs ++ [sd])) subdirs
               return [ulist hfiles `addAttr` ("style","list-style: none")]
    htmldoc <- subdirHtmlPage navobj (home,brand) d
                [h1 [smallMutedText navobj]] doc 
    let htmlsrcfile = basedir </> "index.html"
    writeReadableFile htmlsrcfile htmldoc
    putStrLn $ htmlsrcfile ++ " written"
 where
  isReal fn = not ("." `isPrefixOf` fn) && not (".html" `isSuffixOf` fn) &&
              not (".txt" `isSuffixOf` fn)

  returnExDir f = do exdir <- doesDirectoryExist (basedir </> f)
                     return $ if exdir then [f] else []

  basedir = foldr1 (</>) (base:dirs)

  fileAsHTML cmt filename =
    maybe (return [toHtml filename filename])
          (\n -> if n == "_DUMMY_" ||
                    (last dirs == "operations" && not (isCurryID n))
                   then return []
                   else do fn <- genFromJSON n
                           return [code [hrefInfoBadge fn [htxt n]]])
          (jsonFile2Name filename)
   where
    toHtml ref name = code [href ref [htxt name]]

    genFromJSON n
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
        putStrLn $ "Generating infos for object: " ++ prettyObject obj
        let htmlopts = opts { optShowAll = True, optOutFormat = OutText
                            , optColor = True }
        infotxt <- resultText <$> getInfos htmlopts obj []
        let navobj = prettyObject obj
        docs <- subdirHtmlPage navobj (home,brand) d
                  [h1 [smallMutedText navobj]] (structureText infotxt)
        let htmlfile = encodeFilePath n <.> "html"
        writeReadableFile (basedir </> htmlfile) docs
        return htmlfile

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
subdirHtmlPage :: String -> (String,[BaseHtml]) -> Int -> [BaseHtml]
               -> [BaseHtml] -> IO String
subdirHtmlPage pagetitle homebrand d header maindoc = do
  time <- getLocalTime
  let btbase = concat (take d (repeat "../")) ++ "bt4"
  return $ showHtmlPage $
    bootstrapPage (favIcon btbase) (cssIncludes btbase) (jsIncludes btbase)
      pagetitle homebrand leftTopMenu rightTopMenu 0 [] header
      maindoc (curryDocFooter time)


-- This operation returns the requested object from the given options.
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

--- Writes a file readable for all:
writeReadableFile :: String -> String -> IO ()
writeReadableFile f s =
  writeFile f s >> system ("chmod 644 '" ++ f ++ "'") >> return ()

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
leftTopMenu = []

--- The standard right top menu.
rightTopMenu :: [[BaseHtml]]
rightTopMenu =
  [ [ehrefNav masalaHomeURL [htxt "Masala"]]
  , [ehrefNav cpmHomeURL    [htxt "Curry Package Manager"]]
  , [ehrefNav curryHomeURL  [htxt "Curry Homepage"]]
  ]
 where
  masalaHomeURL = "https://cpm.curry-lang.org/masala/run.cgi"

-- Standard footer information for generated web pages:
curryDocFooter :: CalendarTime -> [BaseHtml]
curryDocFooter time =
  [italic [htxt "Generated by CurryInfo at ",
           htxt (calendarTimeToString time)]]

------------------------------------------------------------------------------
