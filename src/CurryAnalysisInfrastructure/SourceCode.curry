module CurryAnalysisInfrastructure.SourceCode where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Checkout (checkoutIfMissing)
import CurryAnalysisInfrastructure.Options (Options, fullVerbosity, testOptions)
import CurryAnalysisInfrastructure.Verbosity (printLine, printDebugMessage)

import System.CurryPath (modNameToPath)
import System.Directory (doesFileExist)

import Data.List (isPrefixOf, isInfixOf, groupBy, last, elemIndex, findIndex)
import Data.Maybe (fromMaybe)

type Checker = String -> Bool

-- This operation looks for the line in which the given definition starts.
findDefinition :: Checker -> String -> Maybe Int
findDefinition check content = findIndex check (lines content)

-- This operation determines whether a line belongs to a definition.
belongs :: String -> Bool
belongs l = isPrefixOf " " l || isPrefixOf "\t" l || null l

-- This operation reads the source file of the given module and returns the path to the source file
-- and the read content of that file.
readSourceFile :: Options -> Package -> Version -> Module -> IO (Maybe (String, String))
readSourceFile opts pkg vsn m = do
    printLine opts
    printDebugMessage opts "Reading source file..."
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Nothing -> do
            printDebugMessage opts "Failed to read source file"
            return Nothing
        Just cpath -> do
            let path = cpath ++ "/src/" ++ modNameToPath m ++ ".curry"
            printDebugMessage opts $ "Path to source file: " ++ path
            b <- doesFileExist path
            case b of
                False -> do
                    printDebugMessage opts $ "Source file does not exist: " ++ path
                    return Nothing
                True -> do
                    printDebugMessage opts "Reading content..."
                    content <- readFile path
                    return (Just (path, content))
                    --return (Just content)

-- This operation looks for the part of the source code that corresponds to the given checker and returns a reference to that part.
takeSourceCode :: Options -> Checker -> (String -> Bool) -> String -> String -> IO (Maybe Reference)
takeSourceCode opts check belong path content = do
    printLine opts
    printDebugMessage opts "Taking source code..."
    case findDefinition check content of
        Nothing -> do
            printDebugMessage opts "Could not find definition."
            return Nothing
        Just i -> do
            let ls = lines content
            case drop i ls of
                [] -> do
                    printDebugMessage opts "Could not find definition."
                    return Nothing
                (x:xs) -> do
                    let source = x : takeWhile belong xs
                    return (Just (path, i, i + length source))

-- This operation looks for the documentation that corresponds to the given checker and returns a reference to that part.
takeDocumentation :: Options -> Checker -> String -> String -> IO (Maybe Reference)
takeDocumentation opts check path content = do
    printLine opts
    printDebugMessage opts "Taking documentation..."
    case findDefinition check content of
        Nothing -> do
            printDebugMessage opts "Could not find definition."
            return Nothing
        Just i -> do
            let ls = lines content
            case reverse (takeWhile (isPrefixOf "--") (reverse (take i ls))) of
                [] -> do
                    printDebugMessage opts "Could not find documentation."
                    return Nothing
                gs -> do
                    return (Just (path, i - length gs, i))

class SourceCode a where
    readSourceCode :: Options -> a -> IO (Maybe Reference)
    readDocumentation :: Options -> a -> IO (Maybe Reference)

instance SourceCode CurryModule where
    readSourceCode opts (CurryModule pkg vsn m) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                let ls = lines content
                let (tmp, source) = span (isPrefixOf "--") ls
                return (Just (path, length tmp, length ls))
    
    readDocumentation opts (CurryModule pkg vsn m) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                let ls = lines content
                let (doc, _) = span (isPrefixOf "--") ls
                return (Just (path, 0, length doc))

-- This operation returns a checker that look for the definition of the given type.
checkType :: Type -> Checker
checkType t l = (isPrefixOf ("data " ++ t) l || isPrefixOf ("type " ++ t) l || isPrefixOf ("newtype " ++ t) l)

instance SourceCode CurryType where
    readSourceCode opts (CurryType pkg vsn m t) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                takeSourceCode opts (checkType t) belongs path content
    
    readDocumentation opts (CurryType pkg vsn m t) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                takeDocumentation opts (checkType t) path content

-- This operation returns a checker that look for the definition of the given typeclass.
checkTypeclass :: Typeclass -> Checker
checkTypeclass c l = let
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

instance SourceCode CurryTypeclass where
    readSourceCode opts (CurryTypeclass pkg vsn m c) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                takeSourceCode opts (checkTypeclass c) belongs path content
    
    readDocumentation opts (CurryTypeclass pkg vsn m c) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                takeDocumentation opts (checkTypeclass c) path content

-- This operation returns a checker that look for the definition of the given operation.
checkOperation :: Operation -> Checker
checkOperation o l = let
        ls = words l
        operationIndex = elemIndex o ls
        paranthesisIndex = elemIndex ("(" ++ o ++ ")") ls
        typingIndex = elemIndex "::" ls
        equalIndex = elemIndex "=" ls
    in
        if elem o ls && (elem "::" ls || elem "=" ls)
            then
                fromMaybe False ((<) <$> operationIndex <*> typingIndex) ||
                fromMaybe False ((<) <$> operationIndex <*> equalIndex) ||
                fromMaybe False ((<) <$> paranthesisIndex <*> typingIndex) ||
                fromMaybe False ((<) <$> paranthesisIndex <*> equalIndex)
            else False

instance SourceCode CurryOperation where
    readSourceCode opts (CurryOperation pkg vsn m o) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                takeSourceCode opts (checkOperation o) (\l -> belongs l || checkOperation o l || isPrefixOf "#" l) path content
    
    readDocumentation opts (CurryOperation pkg vsn m o) = do
        mresult <- readSourceFile opts pkg vsn m
        case mresult of
            Nothing -> do
                return Nothing
            Just (path, content) -> do
                takeDocumentation opts (checkOperation o) path content