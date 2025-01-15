------------------------------------------------------------------------------
--- This modules defines operations to extract information from Curry interface
--- files.
------------------------------------------------------------------------------

module CurryInfo.Interface where

import Data.List             ( find, intercalate )

import System.Directory     ( doesFileExist, getCurrentDirectory
                            , setCurrentDirectory )
import System.IOExts        ( evalCmd )
import System.FrontendExec  ( FrontendTarget (..), callFrontend )
import System.CurryPath     ( currySubdir )
import System.FilePath      ( (</>), (<.>) )
import Text.Pretty          ( pPrint )

import CurryInfo.Checkout   ( getCheckoutPath )
import CurryInfo.SourceCode ( getSourceFilePath )
import CurryInfo.Types
import CurryInfo.Commands   ( runCmd, cmdCPMInstall, cmdCurryLoad )
import CurryInfo.Verbosity  ( printStatusMessage, printDetailMessage
                            , printDebugMessage, printDebugString )

import CurryInfo.RequestTypes
import CurryInterface.Types
import CurryInterface.Files  ( curryInterfaceFileName, readCurryInterfaceFile )
import CurryInterface.Pretty ( defaultOptions, ppConstructor, ppNewConstructor
                             , ppType, ppMethodDecl, ppQualType )

-- This action tries to parse the respective .icurry file. If the file
-- does not exist yet, the action will invoke 'cypm' and 'curry' to install
-- missing dependencies of the package and make the parser generate
-- the .icurry file.
readInterface :: Options -> Package -> Version -> Module -> IO (Maybe Interface)
readInterface opts pkg vsn m = do
  mbsrcpath <- getSourceFilePath opts pkg vsn m
  case mbsrcpath of
    Nothing -> return Nothing
    Just (srcdir,_) -> do
      path <- getCheckoutPath pkg vsn
      printDebugMessage opts "Path to icurry file is:"
      let icurry = srcdir </> (curryInterfaceFileName m)
      printDebugMessage opts icurry
      printDebugString opts "Checking whether icurry file exists..."
      b <- doesFileExist icurry
      case b of
        True  -> printDebugMessage opts "ok"
        False -> do
          printDebugMessage opts "File does not exist!"
          printDebugMessage opts "Generating icurry file..."
          runCmd opts (cmdCPMInstall opts path)
          runCmd opts (cmdCurryLoad opts path m)
          return ()
      printDebugMessage opts "Reading interface..."
      fmap Just (readCurryInterfaceFile icurry)

-- This operation returns the declarations of the given interface.
getDeclarations :: Interface -> [IDecl]
getDeclarations (Interface _ _ decls) = decls

-- TYPE

-- This operation returns all declarations of types, including types
-- that are hidden.
getAllTypes :: [IDecl] -> [IDecl]
getAllTypes = filter isType

--- This operation returns True, if the given declaration is the declaration
--- of a type. Otherwise it returns False.
isType :: IDecl -> Bool
isType decl = case decl of
  IDataDecl _ _ _ _ _     -> True
  INewtypeDecl _ _ _ _ _  -> True
  ITypeDecl _ _ _ _       -> True
  _                       -> False

-- This operation finds the declaration of the given type.
getTypeDecl :: String -> [IDecl] -> Maybe IDecl
getTypeDecl t = find (\decl -> Just t == (getTypeQName decl >>= Just . snd))

--- This operation returns the qualified name of the type of the given
--- type declaration. The module name is the empty string if it is defined
--- in the given module, otherwise it is re-exported and defined in another
--- module.
--- If the given declaration is not of a type, Nothing is returned.
getTypeQName :: IDecl -> Maybe (String,String)
getTypeQName decl = case decl of
  IDataDecl name _ _ _ _      -> Just $ fromQId name
  INewtypeDecl name _ _ _ _   -> Just $ fromQId name
  ITypeDecl name _ _ _        -> Just $ fromQId name
  _                           -> Nothing

--- This operation returns the constructors of the given type declaration.
--- If the declaration is not of a type, Nothing is returned.
getTypeConstructors :: IDecl -> Maybe [Constructor]
getTypeConstructors decl = case decl of
  IDataDecl _ _ _ cs _   -> Just $ map (pPrint . ppConstructor defaultOptions) cs
  INewtypeDecl _ _ _ c _ -> Just [(pPrint . ppNewConstructor defaultOptions) c]
  ITypeDecl _ _ _ t      -> Just [(pPrint . ppType defaultOptions 0) t]
  _                      -> Nothing

--- This operation returns all declarations of types to be hidden.
getHiddenTypes :: [IDecl] -> [IDecl]
getHiddenTypes = filter isHiddenType

--- This operation returns `True` if the given declaration is the declaration
--- of a hiding type. Otherwise `False` is returned.
isHiddenType :: IDecl -> Bool
isHiddenType decl = case decl of
  HidingDataDecl _ _ _ -> True
  _                    -> False

--- This operation returns the qualified name of the type of the given hiding
--- declaration. The module name is the empty string if it is defined
--- in the given module, otherwise it is re-exported and defined in another
--- module.
--- If the given declaration is not of a hiding type, Nothing is returned.
getHiddenTypeQName :: IDecl -> Maybe (String,String)
getHiddenTypeQName decl = case decl of
  HidingDataDecl name _ _ -> Just $ fromQId name
  _                       -> Nothing

-- TYPECLASS

--- This operations returns all type class declarations of the given list
--- of declarations.
getAllClasses :: [IDecl] -> [IDecl]
getAllClasses = filter isClass

--- This operation returns True, if the given declaration declares a type class.
--- Otherwise it returns False.
isClass :: IDecl -> Bool
isClass decl = case decl of
  IClassDecl _ _ _ _ _ _ _  -> True
  _                         -> False

-- This operation finds the declaration of the given type class.
getClassDecl :: String -> [IDecl] -> Maybe IDecl
getClassDecl c = find (\decl -> Just c == (getClassQName decl >>= Just . snd))

--- This operation returns the qualified name of a type class from its
--- declaration. The module name is the empty string if it is defined
--- in the given module, otherwise it is re-exported and defined in another
--- module.
--- If the given declaration is not of a type class, Nothing is returned.
getClassQName :: IDecl -> Maybe (String,String)
getClassQName decl = case decl of
  IClassDecl _ name _ _ _ _ _   -> Just $ fromQId name
  _                             -> Nothing

-- This operation returns the methods of a type class from its declaration. If the given
-- declaration is not of a type class, Nothing is returned.
getClassMethods :: IDecl -> Maybe [Method]
getClassMethods decl = case decl of
  IClassDecl _ _ _ _ _ methods _  -> Just $ map (pPrint . ppMethodDecl defaultOptions) methods
  _                               -> Nothing

-- This operation returns the declarations of hiding classes from the given declarations.
getHiddenClasses :: [IDecl] -> [IDecl]
getHiddenClasses = filter isHiddenClass

-- This operation returns True, if the given declaration declares a hiding class.
-- Otherwise it returns False.
isHiddenClass :: IDecl -> Bool
isHiddenClass decl = case decl of
  HidingClassDecl _ _ _ _ _ -> True
  _                         -> False

--- This operation returns the qualified name of a type class from a hiding
--- class declaration. The module name is the empty string if it is defined
--- in the given module, otherwise it is re-exported and defined in another
--- module.
--- If the given declaration is not of a hiding class, Nothing is returned.
getHiddenClassQName :: IDecl -> Maybe (String,String)
getHiddenClassQName decl = case decl of
  HidingClassDecl _ name _ _ _ -> Just $ fromQId name
  _                            -> Nothing

-- OPERATION

-- This operation returns the declarations of operations from the
-- given declarations.
getOperations :: [IDecl] -> [IDecl]
getOperations = filter isOperation

-- This operation returns True, if the given declaration declares an operation.
-- Otherwise it returns False.
isOperation :: IDecl -> Bool
isOperation decl = case decl of
  IFunctionDecl _ _ _ _   -> True
  _                       -> False

-- This operation finds the declaration of the given operation.
getOperationDecl :: String -> [IDecl] -> Maybe IDecl
getOperationDecl o =
  find (\decl -> Just o == (getOperationQName decl >>= Just . snd))

--- This operation returns the qualified name of the given operation
--- declaration. The module name is the empty string if it is defined
--- in the given module, otherwise it is re-exported and defined in another
--- module.
--- If the given declaration is not of an operation, Nothing is returned.
getOperationQName :: IDecl -> Maybe (String,String)
getOperationQName decl = case decl of
  IFunctionDecl name _ _ _    -> Just $ fromQId name
  _                           -> Nothing

-- This operation returns the arity from the given operation declaration.
-- If the given declaration is not of an operation, Nothing is returned.
getOperationArity :: IDecl -> Maybe Int
getOperationArity decl = case decl of
  IFunctionDecl _ _ arity _   -> Just arity
  _                           -> Nothing

-- This operation returns the signature of the given operation declaration.
-- If the given declaration is not of an operation, Nothing is returned.
getOperationSignature :: IDecl -> Maybe Signature
getOperationSignature decl = case decl of
  IFunctionDecl _ _ _ t   -> Just $ (pPrint . ppQualType defaultOptions) t
  _                       -> Nothing

-- This operation finds the infix declaration of the given operation.
getInfixDecl :: Operation -> [IDecl] -> Maybe IDecl
getInfixDecl o = find (\decl -> Just o == getInfixName decl)

-- This operation returns the name of the given infix declaration. If the given
-- declaration is not of an infix, Nothing is returned.
getInfixName :: IDecl -> Maybe String
getInfixName decl = case decl of
  IInfixDecl _ _ name -> Just $ idName $ qidIdent name
  _                   -> Nothing

-- This operation returns the infix of the given infix declaration. If the given
-- declaration is not of an infix, Nothing is returned.
getOperationInfix :: IDecl -> Maybe CurryInfo.RequestTypes.Infix
getOperationInfix decl = case decl of
  IInfixDecl i _ _    -> case i of
    CurryInterface.Types.Infix  -> Just CurryInfo.RequestTypes.Infix
    CurryInterface.Types.InfixL -> Just CurryInfo.RequestTypes.InfixL
    CurryInterface.Types.InfixR -> Just CurryInfo.RequestTypes.InfixR
  _                   -> Nothing

-- This operation returns the precedence of the given infix declaration.
-- If the given declaration ii not of an infix, Nothing is returned.
getOperationPrecedence :: IDecl -> Maybe CurryInfo.RequestTypes.Precedence
getOperationPrecedence decl = case decl of
  IInfixDecl _ p _    -> Just p
  _                   -> Nothing
  
--- Extracts the qualified name from a given `QualIdent`.
fromQId :: QualIdent -> (String,String)
fromQId name =
  let ename = idName $ qidIdent name
  in maybe ("", ename)
           (\mi -> (intercalate "." $ midQualifiers mi, ename))
           (qidModule name)

