------------------------------------------------------------------------------
--- This modules defines operations to create output for requests
--- w.r.t. some options.
------------------------------------------------------------------------------

module CurryInfo.Printer where

import Data.Char           ( toLower )
import Data.List           ( init, last )
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
pPackageVersions opts vsns = pStringList opts vsns

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
pVersionCategories opts cats = pStringList opts cats

pVersionModules :: Printer [String]
pVersionModules opts mods = pStringList opts mods

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
pTypeConstructors _ cons = return (show cons)

pTypeDefinition :: Printer Reference
pTypeDefinition = printSourceCode

-- TYPECLASS

pClassName :: Printer Class
pClassName _ = return

pClassDocumentation :: Printer Reference
pClassDocumentation  = printDocumentation

pClassMethods :: Printer [Method]
pClassMethods _ ms = return (show ms)

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
pOperationSignature _ = return

pOperationInfix :: Printer (Maybe Infix)
pOperationInfix opts inf
  | isTextFormat opts = return $ maybe "no fixity defined" (map toLower . show)
                                       inf
  | otherwise         = return $ show inf

pOperationPrecedence :: Printer (Maybe Precedence)
pOperationPrecedence opts p
  | isTextFormat opts = return $ maybe "no precedence defined" show p
  | otherwise         = return $ show p

pOperationCASSDeterministic :: Printer String
pOperationCASSDeterministic opts s
  | isTextFormat opts = return $ maybeRead s prettyDeterministic s
  | otherwise         = return s

pOperationCASSDemand :: Printer String
pOperationCASSDemand opts s
  | isTextFormat opts = return $ maybeRead s prettyDemand s
  | otherwise         = return s
 where
  prettyDemand :: [Int] -> String
  prettyDemand ds = case ds of
    []  -> "no demanded arguments"
    [d] -> "argument " ++ show d
    _   -> "arguments " ++ unwords (map show ds)

pOperationCASSIndeterministic :: Printer String
pOperationCASSIndeterministic opts s
  | isTextFormat opts = return $ maybeRead s prettyIndet s
  | otherwise         = return s
 where
  prettyIndet True  = "might be indeterministic"
  prettyIndet False = "referentially transparent operation"

pOperationCASSSolComplete :: Printer String
pOperationCASSSolComplete opts s
  | isTextFormat opts = return $ maybeRead s prettySolComplete s
  | otherwise         = return s
 where
  prettySolComplete True  = "operationally complete operation"
  prettySolComplete False = "operation might suspend on free variables"

pOperationCASSTerminating :: Printer String
pOperationCASSTerminating opts s
  | isTextFormat opts = return $ maybeRead s prettyTerminate s
  | otherwise         = return s
 where
  prettyTerminate True  = "yes"
  prettyTerminate False = "possibly non-terminating"

pOperationCASSTotal :: Printer String
pOperationCASSTotal opts s
  | isTextFormat opts = return $ maybeRead s prettyTotal s
  | otherwise         = return s
 where
  prettyTotal True  = "reducible on all ground data terms"
  prettyTotal False = "possibly non-reducible on same data term"

pOperationCASSValues :: Printer String
pOperationCASSValues opts s
  | isTextFormat opts = return $ maybeRead s prettyAType s
  | otherwise         = return s

pOperationFailFree :: Printer String
pOperationFailFree opts s
  | isTextFormat opts
  = case s of
     c1:c2:cs | [c1,c2] == "0:" -> return $ maybeRead cs showACallType cs
              | [c1,c2] == "1:" -> return $ maybeRead cs showFuncDeclAsLambda cs
     _                          -> return s
  | otherwise
  = return s

pOperationIOType :: Printer String
pOperationIOType opts s
  | isTextFormat opts = return $ maybeRead s showIOT s
  | otherwise         = return s

------------------------------------------------------------------------------
-- Auxililiaries.

-- Is the output format `Text`?
isTextFormat :: Options -> Bool
isTextFormat opts = optOutFormat opts == OutText

-- Print a list of string as words if output format is `Text`.
pStringList :: Options -> [String] -> IO String
pStringList opts ws =
  return $ if isTextFormat opts then unwords ws
                                else show ws

-- Pretty print a documentation string.
-- If the output mode is text, the leading comment characters are deleted.
printDocumentation :: Options -> Reference -> IO String
printDocumentation opts ref =
  fmap (if isTextFormat opts
          then concatMap ("\n" ++) . map stripDocCmt . lines
          else id)
       (printFromReference opts ref)
 where
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
             then "\n" ++ removeLastCR slice
             else slice
 where
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
