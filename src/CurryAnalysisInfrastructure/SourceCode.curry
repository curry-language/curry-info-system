module CurryAnalysisInfrastructure.SourceCode where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Checkout (checkoutIfMissing)
import CurryAnalysisInfrastructure.Options (Options, fullVerbosity, testOptions)
import CurryAnalysisInfrastructure.Verbosity (printLine, printDebugMessage)

import System.CurryPath (modNameToPath)
import System.Directory (doesFileExist)

import Data.List (isPrefixOf, isInfixOf, groupBy, last, elemIndex, findIndex)
import Data.Maybe (fromMaybe)

findDefinition :: (String -> Bool) -> String -> Maybe Int
findDefinition check content = findIndex check (lines content)

belongs :: String -> Bool
belongs l = isPrefixOf " " l || isPrefixOf "\t" l || null l

readSourceFile :: Options -> Package -> Version -> Module -> IO (Maybe String)
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
                    return (Just content)

takeSourceCode :: Options -> (String -> Bool) -> (String -> Bool) -> String -> IO (Maybe String)
takeSourceCode opts check belong content = do
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
                    let source = unlines (x : takeWhile belong xs)
                    return (Just source) 

takeDocumentation :: Options -> (String -> Bool) -> String -> IO (Maybe String)
takeDocumentation opts check content = do
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
                    return (Just (unlines gs))

class SourceCode a where
    readSourceCode :: Options -> a -> IO (Maybe String)
    readDocumentation :: Options -> a -> IO (Maybe String)

instance SourceCode CurryModule where
    readSourceCode opts (CurryModule pkg vsn m) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                let source = unlines (dropWhile (isPrefixOf "--") (lines content))
                return (Just source)
    
    readDocumentation opts (CurryModule pkg vsn m) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                let doc = unlines (takeWhile (isPrefixOf "--") (lines content))
                return (Just doc)

checkType :: Type -> String -> Bool
checkType t l = (isPrefixOf ("data " ++ t) l || isPrefixOf ("type " ++ t) l || isPrefixOf ("newtype " ++ t) l)

instance SourceCode CurryType where
    readSourceCode opts (CurryType pkg vsn m t) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                takeSourceCode opts (checkType t) belongs content
    
    readDocumentation opts (CurryType pkg vsn m t) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                takeDocumentation opts (checkType t) content

checkTypeclass :: Typeclass -> String -> Bool
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
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                takeSourceCode opts (checkTypeclass c) belongs content
    
    readDocumentation opts (CurryTypeclass pkg vsn m c) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                takeDocumentation opts (checkTypeclass c) content

checkOperation :: Operation -> String -> Bool
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
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                takeSourceCode opts (checkOperation o) (\l -> belongs l || checkOperation o l || isPrefixOf "#" l) content
    
    readDocumentation opts (CurryOperation pkg vsn m o) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                takeDocumentation opts (checkOperation o) content

{-
test :: IO ()
test = do
    let input = CurryOperation "base" "3.2.0" "Prelude" "liftM2"
    mdoc <- readDocumentation testOptions input
    msource <- readSourceCode testOptions input
    case mdoc of
        Nothing -> putStrLn "DOC FAILED"
        Just doc -> do
            putStrLn "-----------------------------------------"
            putStrLn "DOC"
            putStrLn doc
            putStrLn "-----------------------------------------"
    case msource of
        Nothing -> putStrLn "SOURCE FAILED"
        Just source -> do
            putStrLn "-----------------------------------------"
            putStrLn "SOURCE"
            putStrLn source
            putStrLn "-----------------------------------------"
-}