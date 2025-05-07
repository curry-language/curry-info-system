------------------------------------------------------------------------------
--- This modules defines operations to create output for requests
--- w.r.t. some options.
------------------------------------------------------------------------------

module CurryInfo.Printer where

import Data.Char           ( toLower )
import Data.List           ( init, last, sort )
import System.Directory    ( doesFileExist )
import System.IOExts       ( readCompleteFile )

import JSON.Data

import CurryInfo.Helper       ( parenthesize, quote, readSliceFromFile
                              , safeRead )
import CurryInfo.Paths        ( addRootPath )
import CurryInfo.RequestTypes
import CurryInfo.Types
import CurryInfo.Verbosity    ( printStatusMessage, printDetailMessage
                              , printDebugMessage, printErrorMessage )

------------------------------------------------------------------------------

--- A `Printer` is an operation to show information stored in CurryInfo
--- in a human-friendly string representation.
--- The `Options` parameter allows to print status/debug messages
--- to inform the user about the progress, or to produce different
--- output strings according to the output format.
--- The second argument of type `b` is the information stored by CurryInfo
--- to be printed.
type Printer b = Options -> b -> IO String

------------------------------------------------------------------------------
-- PACKAGE

pPackageName :: Printer Package
pPackageName _ s = return s

pPackageVersions :: Printer [Version]
pPackageVersions opts vsns = pSortedStringList opts vsns

-- VERSION

pVersionVersion :: Printer Version
pVersionVersion _ vsn = return vsn

pVersionDocumentation :: Printer String
pVersionDocumentation opts vpath =
  if null vpath
    then return "" -- no README file in package
    else do
      let path = addRootPath opts vpath
      b <- doesFileExist path
      case b of
        False -> returnFileError path
        True -> do
          printDebugMessage opts $ "Reading from file '" ++ path ++ "'..."
          vdoc <- readCompleteFile path
          return $ (if isTextFormat opts then "\n" else "") ++ vdoc

pVersionCategories :: Printer [Category]
pVersionCategories opts cats = pSortedStringList opts cats

pVersionModules :: Printer [String]
pVersionModules opts mods = pSortedStringList opts mods

pVersionDependencies :: Printer [Dependency]
pVersionDependencies _ deps = return (show deps)
  
-- MODULE

pModuleName :: Printer Module
pModuleName _ name = return name

pModuleDocumentation :: Printer Reference
pModuleDocumentation = printDocumentation

pModuleSourceCode :: Printer Reference
pModuleSourceCode = printSourceCode

pModuleUnsafeModule :: Printer String
pModuleUnsafeModule opts safe
  | isTextFormat opts = return $ maybeRead safe showUnsafeMods safe
  | otherwise         = return safe
 where
  showUnsafeMods ms =
    if null ms then "safe"
               else "unsafe due to module" ++
                    (if length ms > 1 then "s " else " ") ++ unwords ms

pModuleClasses :: Printer [Class]
pModuleClasses opts cs = pStringList opts cs

pModuleTypes :: Printer [Type]
pModuleTypes opts ts = pStringList opts ts

pModuleOperations :: Printer [Operation]
pModuleOperations opts os = pStringList opts os

-- TYPE

pTypeName :: Printer Type
pTypeName _ = return

pTypeDocumentation :: Printer Reference
pTypeDocumentation = printDocumentation

pTypeConstructors :: Printer [Constructor]
pTypeConstructors opts cs
  | isTextFormat opts = return$ codeText opts (show cs)
  | otherwise         = return (show cs)

pTypeDefinition :: Printer Reference
pTypeDefinition = printSourceCode

-- CLASS

pClassName :: Printer Class
pClassName _ = return

pClassDocumentation :: Printer Reference
pClassDocumentation  = printDocumentation

pClassMethods :: Printer [Method]
pClassMethods opts ms
  | isTextFormat opts = return $ codeText opts (show ms)
  | otherwise         = return (show ms)

pClassDefinition :: Printer Reference
pClassDefinition = printSourceCode

-- OPERATION

pOperationName :: Printer Operation
pOperationName _ = return

pOperationDocumentation :: Printer Reference
pOperationDocumentation  = printDocumentation

pOperationSourceCode :: Printer Reference
pOperationSourceCode = printSourceCode

pOperationSignature :: Printer Signature
pOperationSignature opts s
  | isTextFormat opts = return $ codeText opts s
  | otherwise         = return s

pOperationInfix :: Printer (Maybe Infix)
pOperationInfix opts inf
  | isTextFormat opts = return $ italicText opts $
                          maybe "no fixity defined" (map toLower . show) inf
  | otherwise         = return $ show inf

pOperationPrecedence :: Printer (Maybe Precedence)
pOperationPrecedence opts p
  | isTextFormat opts = return $ italicText opts $
                          maybe "no precedence defined" show p
  | otherwise         = return $ show p

pOperationCASSDeterministic :: Printer String
pOperationCASSDeterministic opts s = returnItalicText opts prettyDeterministic s

pOperationCASSDemand :: Printer String
pOperationCASSDemand opts s = returnItalicText opts prettyDemand s
 where
  prettyDemand :: [Int] -> String
  prettyDemand ds = case ds of
    []  -> "no demanded arguments"
    [d] -> "argument " ++ show d
    _   -> "arguments " ++ unwords (map show ds)

pOperationCASSIndeterministic :: Printer String
pOperationCASSIndeterministic opts s = returnItalicText opts prettyIndet s
 where
  prettyIndet True  = "might be indeterministic"
  prettyIndet False = "referentially transparent operation"

pOperationCASSSolComplete :: Printer String
pOperationCASSSolComplete opts s = returnItalicText opts prettySolComplete s
 where
  prettySolComplete True  = "operationally complete operation"
  prettySolComplete False = "operation might suspend on free variables"

pOperationCASSTerminating :: Printer String
pOperationCASSTerminating opts s = returnItalicText opts prettyTerminate s
 where
  prettyTerminate True  = "yes"
  prettyTerminate False = "possibly non-terminating"

pOperationCASSTotal :: Printer String
pOperationCASSTotal opts s = returnItalicText opts prettyTotal s
 where
  prettyTotal True  = "reducible on all ground data terms"
  prettyTotal False = "possibly non-reducible on same data term"

pOperationCASSValues :: Printer String
pOperationCASSValues opts s = returnCodeText opts prettyAType s

pOperationFailFree :: Printer String
pOperationFailFree opts s
  | isTextFormat opts
  = return $ case s of
     c1:c2:cs
       | [c1,c2] == "0:" -> codeText opts $ maybeRead cs showACallType cs
       | [c1,c2] == "1:" -> codeText opts $ maybeRead cs showFuncDeclAsLambda cs
     _                   -> s
  | otherwise
  = return s

pOperationIOType :: Printer String
pOperationIOType opts s = returnCodeText opts showIOT s

-- If the output format is text, read the argument string, pretty print it
-- (with second argument), and return it as code text.
-- Otherwise, just return the argument string.
returnCodeText :: Read a => Options -> (a -> String) -> String -> IO String
returnCodeText opts pp s
  | isTextFormat opts = return $ codeText opts $ maybeRead s pp s
  | otherwise         = return s

-- If the output format is text, read the argument string, pretty print it
-- (with the second argument), and return it as italic text.
-- Otherwise, just return the argument string.
returnItalicText :: Read a => Options -> (a -> String) -> String -> IO String
returnItalicText opts pp s
  | isTextFormat opts = return $ italicText opts $ maybeRead s pp s
  | otherwise         = return s

------------------------------------------------------------------------------
-- Auxililiaries.

--- Show text in code font if markdown option is used.
codeText :: Options -> String -> String
codeText opts s | optMarkdown opts = "`" ++ s ++ "`"
                | otherwise        = s

-- Show text in italic font if markdown option is used.
italicText :: Options -> String -> String
italicText opts s | optMarkdown opts = "_" ++ s ++ "_"
                  | otherwise        = s

-- Is the output format `Text`?
isTextFormat :: Options -> Bool
isTextFormat opts = optOutFormat opts == OutText

-- Print a list of string as words if output format is `Text`, otherwise
-- just show the list.
pStringList :: Options -> [String] -> IO String
pStringList opts ws =
  return $ if isTextFormat opts then unwords ws
                                else show ws

-- Print a list of string as sorted words if output format is `Text`,
-- otherwise just show the list.
pSortedStringList :: Options -> [String] -> IO String
pSortedStringList opts ws =
  return $ if isTextFormat opts then unwords (sort ws)
                                else show ws

-- Pretty print a documentation string.
-- If the output mode is text, the leading comment characters are deleted.
printDocumentation :: Options -> Reference -> IO String
printDocumentation opts ref =
  fmap (if isTextFormat opts
          then if optMarkdown opts
                 then \s -> "  " ++ unlines' (map stripDocCmt (lines s))
                 else unlines' . map stripDocCmt . lines
          else id)
       (printFromReference opts ref)
 where
  unlines' = concatMap ("\n" ++)
  stripDocCmt s | take 4 s == "--- " = drop 4 s
                | take 3 s == "-- "  = drop 3 s
                | take 3 s == "---"  = drop 3 s
                | take 2 s == "--"   = drop 2 s
                | otherwise          = s

-- Pretty print source code from a given reference.
printSourceCode :: Options -> Reference -> IO String
printSourceCode opts ref = do
  slice <- printFromReference opts ref
  return $ if isTextFormat opts
             then "\n" ++ withMarkdown (removeLastCR slice)
             else slice
 where
  withMarkdown s | optMarkdown opts = "```curry\n" ++ s ++ "\n```"
                 | otherwise        = s

  removeLastCR s | null s         = s
                 | last s == '\n' = init s
                 | otherwise      = s

-- This action returns the content of the file the given reference points to.
-- If the file name is empty (used for dummy objects), the empty string
-- is returned.
printFromReference :: Options -> Reference -> IO String
printFromReference opts (Reference rpath start end) =
  if null rpath
    then return ""
    else do
      let path = addRootPath opts rpath
      b <- doesFileExist path
      case b of
        False -> returnFileError path
        True -> do
          printDebugMessage opts $ "Reading from file '" ++ path ++ "'..."
          readSliceFromFile path start end

-- Reads a string and apply the function (second argument) to the value.
-- If there is a read error, return the third argument.
maybeRead :: Read a => String -> (a -> b) -> b -> b
maybeRead s f x = case reads s of [(v,"")] -> f v
                                  _        -> x

-- Shows a file existence error.
returnFileError :: FilePath -> IO String
returnFileError path = do
  printErrorMessage $ "File " ++ quote path ++ " does not exist."
  return $ "FAILED SINCE FILE " ++ quote path ++ " DOES NOT EXIST"

------------------------------------------------------------------------------
