------------------------------------------------------------------------------
--- This modules defines operations to create output for requests
--- w.r.t. some options.
------------------------------------------------------------------------------

module CurryInfo.Printer where

import System.Directory    ( doesFileExist )
import System.IOExts       ( readCompleteFile )

import JSON.Data

import CurryInfo.Helper       ( readSliceFromFile, parenthesize, safeRead )
import CurryInfo.Paths        ( addRootPath, getRoot )
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
  path <- addRootPath vpath
  b <- doesFileExist path
  case b of
    False -> do
      printErrorMessage $ "File '" ++ path ++ "' does not exist."
      return "FAILED DUE TO FILE NOT EXISTING"
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
pModuleDocumentation opts ref = printFromReference opts ref

pModuleSourceCode :: Printer Reference
pModuleSourceCode opts ref = printFromReference opts ref

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
pTypeName _ name = return name

pTypeDocumentation :: Printer Reference
pTypeDocumentation opts ref = printFromReference opts ref

pTypeConstructors :: Printer [Constructor]
pTypeConstructors _ cons = return (show cons)

pTypeDefinition :: Printer Reference
pTypeDefinition opts ref = printFromReference opts ref

-- TYPECLASS

pClassName :: Printer Class
pClassName _ name = return name

pClassDocumentation :: Printer Reference
pClassDocumentation opts ref = printFromReference opts ref

pClassMethods :: Printer [Method]
pClassMethods _ ms = return (show ms)

pClassDefinition :: Printer Reference
pClassDefinition opts ref = printFromReference opts ref

-- OPERATION

pOperationName :: Printer Operation
pOperationName _ name = return name

pOperationDocumentation :: Printer Reference
pOperationDocumentation opts ref = printFromReference opts ref

pOperationSourceCode :: Printer Reference
pOperationSourceCode opts ref = printFromReference opts ref

pOperationSignature :: Printer Signature
pOperationSignature _ s = return s

pOperationInfix :: Printer (Maybe Infix)
pOperationInfix _ inf = return (show inf)

pOperationPrecedence :: Printer (Maybe Precedence)
pOperationPrecedence _ p = return (show p)

pOperationCASSDeterministic :: Printer String
pOperationCASSDeterministic _ det = return det

pOperationCASSDemand :: Printer String
pOperationCASSDemand _ dem = return dem

pOperationCASSIndeterministic :: Printer String
pOperationCASSIndeterministic _ ind = return ind

pOperationCASSSolComplete :: Printer String
pOperationCASSSolComplete _ sol = return sol

pOperationCASSTerminating :: Printer String
pOperationCASSTerminating _ t = return t

pOperationCASSTotal :: Printer String
pOperationCASSTotal _ t = return t

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

-- This action returns the content of the file the given reference points to.
printFromReference :: Options -> Reference -> IO String
printFromReference opts (Reference rpath start end) = do
  path <- addRootPath rpath
  b <- doesFileExist path
  case b of
    False -> do
      printErrorMessage $ "File '" ++ path ++ "' does not exist."
      return "FAILED DUE TO FILE NOT EXISTING"
    True -> do
      printDebugMessage opts $ "Reading from file '" ++ path ++ "'..."
      slice <- readSliceFromFile path start end
      return $ (if optOutFormat opts == OutText then "\n" else "") ++ slice

-- Reads a string and apply the function (second argument) to the value.
-- If there is a read error, return the third argument.
maybeRead :: Read a => String -> (a -> b) -> b -> b
maybeRead s f x = case reads s of [(v,"")] -> f v
                                  _        -> x

------------------------------------------------------------------------------
