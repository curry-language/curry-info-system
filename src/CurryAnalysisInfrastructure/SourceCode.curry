module CurryAnalysisInfrastructure.SourceCode where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.Checkout (checkoutIfMissing)
import CurryAnalysisInfrastructure.Options (Options, fullVerbosity, testOptions)

import System.CurryPath (modNameToPath)
import System.Directory (doesFileExist)

import Data.List (isPrefixOf, isInfixOf, groupBy, last)

import Control.Monad (when)

belongs :: String -> Bool
belongs l = isPrefixOf " " l || isPrefixOf "\t" l || null l

readSourceFile :: Options -> Package -> Version -> Module -> IO (Maybe String)
readSourceFile opts pkg vsn m = do
    mpath <- checkoutIfMissing opts pkg vsn
    case mpath of
        Nothing -> do
            when (fullVerbosity opts) (putStrLn $ "Failed to read source file")
            return Nothing
        Just cpath -> do
            let path = cpath ++ "/src/" ++ modNameToPath m ++ ".curry"
            b <- doesFileExist path
            case b of
                False -> do
                    when (fullVerbosity opts) (putStrLn $ "Source file does not exist: " ++ path)
                    return Nothing
                True -> do
                    content <- readFile path
                    return (Just content)

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
checkType t l = not (isPrefixOf ("data " ++ t) l || isPrefixOf ("type " ++ t) l || isPrefixOf ("newtype " ++ t) l)

instance SourceCode CurryType where
    readSourceCode opts (CurryType pkg vsn m t) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                case dropWhile (checkType t) (lines content) of
                    [] -> do
                        when (fullVerbosity opts) (putStrLn $ "Could not find source code.")
                        return Nothing
                    (x:xs) -> do
                        let source = unlines (x : takeWhile belongs xs)
                        return (Just source)
    
    readDocumentation opts (CurryType pkg vsn m t) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                case takeWhile (checkType t) (lines content) of
                    [] -> do
                        when (fullVerbosity opts) (putStrLn $ "Could not find documentation.")
                        return Nothing
                    xs -> do
                        case groupBy (\l1 l2 -> isPrefixOf "--" l1 && isPrefixOf "--" l2) xs of
                            [] -> do
                                when (fullVerbosity opts) (putStrLn $ "Could not find documentation.")
                                return Nothing
                            gs -> do
                                let docs = last gs
                                return (Just (unlines docs))

checkTypeclass :: Typeclass -> String -> Bool
checkTypeclass c l = not (isPrefixOf ("class " ++ c) l)

instance SourceCode CurryTypeclass where
    readSourceCode opts (CurryTypeclass pkg vsn m c) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                case dropWhile (checkTypeclass c) (lines content) of
                    [] -> do
                        when (fullVerbosity opts) (putStrLn $ "Could not find source code.")
                        return Nothing
                    (x:xs) -> do
                        let source = unlines (x : takeWhile belongs xs)
                        return (Just source)
    
    readDocumentation opts (CurryTypeclass pkg vsn m c) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                case takeWhile (checkTypeclass c) (lines content) of
                    [] -> do
                        when (fullVerbosity opts) (putStrLn $ "Could not find documentation.")
                        return Nothing
                    xs -> do
                        case groupBy (\l1 l2 -> isPrefixOf "--" l1 && isPrefixOf "--" l2) xs of
                            [] -> do
                                when (fullVerbosity opts) (putStrLn $ "Could not find documentation.")
                                return Nothing
                            gs -> do
                                let docs = last gs
                                return (Just (unlines docs))

checkOperation :: Operation -> String -> Bool
checkOperation o l = let ls = words l in
    not (elem o ls && (elem "::" ls || elem "=" ls))

instance SourceCode CurryOperation where
    readSourceCode opts (CurryOperation pkg vsn m o) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                case dropWhile (checkOperation o) (lines content) of
                    [] -> do
                        when (fullVerbosity opts) (putStrLn $ "Could not find source code.")
                        return Nothing
                    (x:xs) -> do
                        let source = unlines (x : takeWhile (\l -> belongs l || checkOperation o l || isPrefixOf "#" l) xs)
                        return (Just source)
    
    readDocumentation opts (CurryOperation pkg vsn m o) = do
        mcontent <- readSourceFile opts pkg vsn m
        case mcontent of
            Nothing -> do
                return Nothing
            Just content -> do
                case takeWhile (checkOperation o) (lines content) of
                    [] -> do
                        when (fullVerbosity opts) (putStrLn $ "Could not find documentation.")
                        return Nothing
                    xs -> do
                        case groupBy (\l1 l2 -> isPrefixOf "--" l1 && isPrefixOf "--" l2) xs of
                            [] -> do
                                when (fullVerbosity opts) (putStrLn $ "Could not find documentation.")
                                return Nothing
                            gs -> do
                                let docs = last gs
                                return (Just (unlines docs))

{-
test :: IO ()
test = do
    let input = CurryOperation "json" "3.0.0" "JSON.Pretty" "ppJSON"
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