-----------------------------------------------------------------------------------------
--- This modules defines operations to extract information from Curry interfaces.
-----------------------------------------------------------------------------------------

module CurryInfo.Interface where

import CurryInfo.Checkout (getCheckoutPath, checkoutIfMissing)
import CurryInfo.Types
import CurryInfo.Commands (runCmd, cmdCYPMInstall, cmdCurryLoad)
import CurryInfo.Verbosity (printStatusMessage, printDetailMessage, printDebugMessage)

import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.IOExts (evalCmd)
import System.FrontendExec (FrontendTarget (..), callFrontend)
import System.CurryPath (currySubdir, modNameToPath)
import System.FilePath ((</>), (<.>))

import CurryInterface.Types
import CurryInterface.Files (readCurryInterfaceFile)
import CurryInterface.Pretty (defaultOptions, ppConstructor, ppNewConstructor, ppType, ppMethodDecl, ppQualType)

import Data.List (find)

import Text.Pretty (pPrint)

-- This action returns the path to the respective .icurry file in the checkouts subdirectory.
icurryPath :: Package -> Version -> Module -> IO String
icurryPath pkg vsn m = do
    path <- getCheckoutPath pkg vsn
    return (path </> "src" </> currySubdir </> modNameToPath m <.> "icurry")

-- This action tries to parse the respective .icurry file. If the file does not exist yet, the action will
-- invoke 'cypm' and 'curry' to install missing dependencies of the package and make the parser generate
-- the .icurry file.
readInterface :: Options -> Package -> Version -> Module -> IO (Maybe Interface)
readInterface opts pkg vsn m = do
    printDetailMessage opts $ "Checkout for version '" ++ vsn ++ "' of package '" ++ pkg ++ "'..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Just path -> do
            printDebugMessage opts "Computing path to icurry file..."
            icurry <- icurryPath pkg vsn m
            printDebugMessage opts $ "Path to icurry file: " ++ icurry
            printDebugMessage opts "Checking whether icurry file exists..."
            b <- doesFileExist icurry
            case b of
                True -> do
                    printDebugMessage opts "icurry file exists."
                    printDebugMessage opts "Reading interface..."
                    result <- readCurryInterfaceFile icurry
                    return $ Just result
                False -> do
                    printDebugMessage opts "icurry file does not exist."
                    printDebugMessage opts "Generating icurry file..."
                    runCmd opts (cmdCYPMInstall path)
                    runCmd opts (cmdCurryLoad path m)
                    printDebugMessage opts "Reading interface..."
                    result <- readCurryInterfaceFile icurry
                    return $ Just result
        Nothing -> do
            printDebugMessage opts "Checkout failed."
            return Nothing

-- This operation returns the declarations of the given interface.
getDeclarations :: Interface -> [IDecl]
getDeclarations (Interface _ _ decls) = decls

-- TYPE

-- This operation returns all declarations of types, including types that are hidden.
getAllTypes :: [IDecl] -> [IDecl]
getAllTypes = filter isType

-- This operation returns True, if the given declaration is the declaration of a type.
-- Otherwise it returns False.
isType :: IDecl -> Bool
isType decl = case decl of
    IDataDecl _ _ _ _ _     -> True
    INewtypeDecl _ _ _ _ _  -> True
    ITypeDecl _ _ _ _       -> True
    _                       -> False

-- This operation finds the declaration of the given type.
getTypeDecl :: String -> [IDecl] -> Maybe IDecl
getTypeDecl t = find (\decl -> Just t == getTypeName decl)

-- This operation returns the name of the type of the given declaration. If the given
-- declaration is not of a type, Nothing is returned.
getTypeName :: IDecl -> Maybe String
getTypeName decl = case decl of
    IDataDecl name _ _ _ _      -> Just $ idName $ qidIdent name
    INewtypeDecl name _ _ _ _   -> Just $ idName $ qidIdent name
    ITypeDecl name _ _ _        -> Just $ idName $ qidIdent name
    _                           -> Nothing

-- This operation returns the constructors of the given type declaration. If the declaration
-- is not of a type, Nothing is returned.
getTypeConstructors :: IDecl -> Maybe [Constructor]
getTypeConstructors decl = case decl of
    IDataDecl _ _ _ constructors _      -> Just $ map (pPrint . ppConstructor defaultOptions) constructors
    INewtypeDecl _ _ _ constructor _    -> Just [(pPrint . ppNewConstructor defaultOptions) constructor]
    ITypeDecl _ _ _ t                   -> Just [(pPrint . ppType defaultOptions 0) t]
    _                                   -> Nothing

-- This operation returns all declarations of types to be hidden.
getHiddenTypes :: [IDecl] -> [IDecl]
getHiddenTypes = filter isHiddenType

-- This operation returns True, if the given declaration is the declaration of a hiding type.
-- Otherwise it returns False.
isHiddenType :: IDecl -> Bool
isHiddenType decl = case decl of
    HidingDataDecl _ _ _    -> True
    _                       -> False

-- This operation returns the name of the type of the given hiding declaration. If the given
-- declaration is not of a hiding type, Nothing is returned.
getHiddenTypeName :: IDecl -> Maybe String
getHiddenTypeName decl = case decl of
    HidingDataDecl name _ _ -> Just $ idName $ qidIdent name
    _                       -> Nothing

-- TYPECLASS

-- This operations returns all type class declarations of the given list of declarations.
getAllClasses :: [IDecl] -> [IDecl]
getAllClasses = filter isClass

-- This operation returns True, if the given declaration declares a type class.
-- Otherwise it returns False.
isClass :: IDecl -> Bool
isClass decl = case decl of
    IClassDecl _ _ _ _ _ _ _  -> True
    _                         -> False

-- This operation finds the declaration of the given type class.
getClassDecl :: String -> [IDecl] -> Maybe IDecl
getClassDecl c = find (\decl -> Just c == getClassName decl)

-- This operation returns the name of a type class from its declaration. If the given
-- declaration is not of a type class, Nothing is returned.
getClassName :: IDecl -> Maybe String
getClassName decl = case decl of
    IClassDecl _ name _ _ _ _ _   -> Just $ idName $ qidIdent name
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

-- This operation returns the name of a type class from a hiding class declaration. If the given
-- declaration is not of a hiding class, Nothing is returned.
getHiddenClassName :: IDecl -> Maybe String
getHiddenClassName decl = case decl of
    HidingClassDecl _ name _ _ _ -> Just $ idName $ qidIdent name
    _                            -> Nothing

-- OPERATION

-- This operation returns the declarations of operations from the given declarations.
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
getOperationDecl o = find (\decl -> Just o == getOperationName decl)

-- This operation returns the name of the given operation declaration. If the given
-- declaration is not of an operation, Nothing is returned.
getOperationName :: IDecl -> Maybe String
getOperationName decl = case decl of
    IFunctionDecl name _ _ _    -> Just $ idName $ qidIdent name
    _                           -> Nothing

-- This operation returns the arity from the given operation declaration. If the given
-- declaration is not of an operation, Nothing is returned.
getOperationArity :: IDecl -> Maybe Int
getOperationArity decl = case decl of
    IFunctionDecl _ _ arity _   -> Just arity
    _                           -> Nothing

-- This operation returns the signature of the given operation declaration. If the given
-- declaration is not of an operation, Nothing is returned.
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
getOperationInfix :: IDecl -> Maybe CurryInfo.Types.Infix
getOperationInfix decl = case decl of
    IInfixDecl i _ _    -> case i of
        CurryInterface.Types.Infix  -> Just CurryInfo.Types.Infix
        CurryInterface.Types.InfixL -> Just CurryInfo.Types.InfixL
        CurryInterface.Types.InfixR -> Just CurryInfo.Types.InfixR
    _                   -> Nothing

-- This operation returns the precedence of the given infix declaration. If the given
-- declaration ii not of an infix, Nothing is returned.
getOperationPrecedence :: IDecl -> Maybe CurryInfo.Types.Precedence
getOperationPrecedence decl = case decl of
    IInfixDecl _ p _    -> Just p
    _                   -> Nothing