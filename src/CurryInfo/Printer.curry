------------------------------------------------------------------------------
--- This modules defines operations to create output for requests
--- w.r.t. some options.
------------------------------------------------------------------------------

module CurryInfo.Printer where

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
pPackageVersions _ vsns = return (show vsns)

-- VERSION

pVersionVersion :: Printer Version
pVersionVersion _ vsn = return vsn

pVersionDocumentation :: Printer String
pVersionDocumentation opts vpath = do
  let path = addRootPath opts vpath
  b <- doesFileExist path
  case b of
    False -> returnFileError path
    True -> do
      printDebugMessage opts $ "Reading from file '" ++ path ++ "'..."
      content <- readCompleteFile path
      return $ (if optOutFormat opts == OutText then "\n" else "") ++ content

pVersionCategories :: Printer [Category]
pVersionCategories _ cats = return (show cats)

pVersionModules :: Printer [String]
pVersionModules _ mods = return (show mods)

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
  | optOutFormat opts == OutText = return $ maybeRead safe showUnsafeMods safe
  | otherwise                    = return safe
 where
  showUnsafeMods ms =
    if null ms then "safe"
               else "unsafe due to module" ++
                    (if length ms > 1 then "s " else " ") ++ unwords ms

pModuleClasses :: Printer [Class]
pModuleClasses _ cs = return (show cs)

pModuleTypes :: Printer [Type]
pModuleTypes _ ts = return (show ts)

pModuleOperations :: Printer [Operation]
pModuleOperations _ os = return (show os)

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
pOperationInfix _ inf = return (show inf)

pOperationPrecedence :: Printer (Maybe Precedence)
pOperationPrecedence _ p = return (show p)

pOperationCASSDeterministic :: Printer String
pOperationCASSDeterministic opts s
  | optOutFormat opts == OutText = return $ maybeRead s prettyDeterministic s
  | otherwise                    = return s

pOperationCASSDemand :: Printer String
pOperationCASSDemand opts s
  | optOutFormat opts == OutText = return $ maybeRead s prettyDemand s
  | otherwise                    = return s
 where
  prettyDemand :: [Int] -> String
  prettyDemand ds = case ds of
    []  -> "no demanded arguments"
    [d] -> "demanded argument position: " ++ show d
    _   -> "demand argument positions: " ++ unwords (map show ds) 

pOperationCASSIndeterministic :: Printer String
pOperationCASSIndeterministic _ = return

pOperationCASSSolComplete :: Printer String
pOperationCASSSolComplete _ = return

pOperationCASSTerminating :: Printer String
pOperationCASSTerminating opts  s
  | optOutFormat opts == OutText = return $ maybeRead s prettyTerminate s
  | otherwise                    = return s
 where
  prettyTerminate True  = "yes"
  prettyTerminate False = "possibly non-terminating"

pOperationCASSTotal :: Printer String
pOperationCASSTotal opts  s
  | optOutFormat opts == OutText = return $ maybeRead s prettyTotal s
  | otherwise                    = return s
 where
  prettyTotal True  = "yes (i.e., always reducible on ground data terms)"
  prettyTotal False = "possibly non-reducible"

pOperationCASSValues :: Printer String
pOperationCASSValues opts s
  | optOutFormat opts == OutText = return $ maybeRead s prettyAType s
  | otherwise                    = return s

pOperationFailFree :: Printer String
pOperationFailFree opts s
  | optOutFormat opts == OutText
  = case s of
     c1:c2:cs | [c1,c2] == "0:" -> return $ maybeRead cs showACallType cs
              | [c1,c2] == "1:" -> return $ maybeRead cs showFuncDeclAsLambda cs
     _                          -> return s
  | otherwise
  = return s

pOperationIOType :: Printer String
pOperationIOType opts s
  | optOutFormat opts == OutText = return $ maybeRead s showIOT s
  | otherwise                    = return s

------------------------------------------------------------------------------
-- Auxililiaries.

-- Pretty print a documentation string.
-- If the output mode is text, the leading comment characters are deleted.
printDocumentation :: Options -> Reference -> IO String
printDocumentation opts ref =
  fmap (if optOutFormat opts == OutText
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
  return $ if optOutFormat opts == OutText
             then "\n" ++ removeLastCR slice
             else slice
 where
  removeLastCR s | null s         = s
                 | last s == '\n' = init s
                 | otherwise      = s

-- This action returns the content of the file the given reference points to.
printFromReference :: Options -> Reference -> IO String
printFromReference opts (Reference rpath start end) = do
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
