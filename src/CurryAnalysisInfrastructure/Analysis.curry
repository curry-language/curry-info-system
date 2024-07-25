module CurryAnalysisInfrastructure.Analysis where

import CurryAnalysisInfrastructure.Commands
import CurryAnalysisInfrastructure.Options
import CurryAnalysisInfrastructure.Types

import JSON.Data
import JSON.Parser (parseJSON)

import Data.List (init, find, intercalate)

import Control.Monad (when)

import DetParse (Parser, parse, word, (<|>), (*>), yield, failure, some, anyChar, char, check, (<$>), (<*>), many, (<*))
import Prelude hiding ((<|>), (*>), some, (<$>), (<*>), many, (<*))

-- Analysis

-- This operation looksup a field with a specific name in a javascript object. If not such field can be found or
-- the javascript value is not a javascript object, then it will return Nothing.
findField :: [JValue] -> String -> Maybe String
findField js field = do
        found <- find (checker field) js
        case found of
            JObject [_, _, ("result", JString result)] -> return result
            _ -> Nothing
    where
        checker :: String -> JValue -> Bool
        checker f1 v = case v of
            JObject [_, ("name", JString f2), _] -> f1 == f2
            _ -> False

-- This action initiates a call to CASS to compute the given analysis for the given module.
-- The parser argument is for parsing the result of the analysis.
analyse :: Options -> String -> String -> Module -> String -> (String -> Maybe a) -> IO (Maybe a)
analyse opts path analysis m field parser = do
    when (fullVerbosity opts) (putStrLn $ "Starting analysis " ++ analysis ++ "...")
    (_, output, _) <- runCmd opts (cmdCASS path analysis m)
    when (fullVerbosity opts) (putStrLn $ "Analysis finished.")
    when (fullVerbosity opts) (putStrLn $ "Parsing result...")
    case parseJSON (init output) of 
        Just (JArray js) -> do
            case findField js field of
                Just result -> do
                    when (fullVerbosity opts) (putStrLn $ "Analysis succeeded.")
                    return $ parser result
                Nothing -> do
                    when (fullVerbosity opts) (putStrLn $ "Could not find entry with name '" ++ field ++ "'.")
                    when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
                    return Nothing
        _ -> do
            when (fullVerbosity opts) (putStrLn $ "Output did not match expected format. Expected array.")
            when (fullVerbosity opts) (putStrLn $ "Output:")
            when (fullVerbosity opts) (putStrLn $ output)
            when (fullVerbosity opts) (putStrLn $ "Analysis failed.")
            return Nothing

-- This action initiates a call to CASS to compute the 'UnsafeModule' analysis for the given module in
-- the given path.
analyseSafeModule :: Options -> String -> Module -> IO (Maybe Safe)
analyseSafeModule opts path m = do
    analyse opts path "UnsafeModule" m m parseSafe

-- This action initiates a call to CASS to compute the 'Deterministic' analysis for the given module in
-- the given path.
analyseDeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Deterministic)
analyseDeterministic opts path m op = do
    analyse opts path "Deterministic" m op parseDeterministic

-- This action initiates a call to CASS to compute the 'Demand' analysis for the given module in
-- the given path.
analyseDemandness :: Options -> String -> Module -> Operation -> IO (Maybe Demandness)
analyseDemandness opts path m op = do
    analyse opts path "Demand" m op parseDemandness

-- This action initiates a call to CASS to compute the 'Indeterministic' analysis for the given module in
-- the given path.
analyseIndeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Indeterministic)
analyseIndeterministic opts path m op = do
    analyse opts path "Indeterministic" m op parseIndeterministic

-- This action initiates a call to CASS to compute the 'SolComplete' analysis for the given module in
-- the given path.
analyseSolutionCompleteness :: Options -> String -> Module -> Operation -> IO (Maybe SolutionCompleteness)
analyseSolutionCompleteness opts path m op = do
    analyse opts path "SolComplete" m op parseSolutionCompleteness

-- This action initiates a call to CASS to compute the 'Terminating' analysis for the given module in
-- the given path.
analyseTermination :: Options -> String -> Module -> Operation -> IO (Maybe Termination)
analyseTermination opts path m op = do
    analyse opts path "Terminating" m op parseTermination

-- This action initiates a call to CASS to compute the 'Total' analysis for the given module in
-- the given path.
analyseTotallyDefined :: Options -> String -> Module -> Operation -> IO (Maybe TotallyDefined)
analyseTotallyDefined opts path m op = do
    analyse opts path "Total" m op parseTotallyDefined

-- PARSER

-- This operation parses the result of the 'UnsafeModule' analysis of CASS.
parseSafe :: String -> Maybe Safe
parseSafe = parse (
        (word "safe" *> yield Safe) <|>
        (word "unsafe" *> (
            (space *> word "(due to module" *> (
                (space *> (UnsafeDue . return <$> parseModule) <* char ')') <|>
                (char 's' *> space *> (UnsafeDue <$> parseList space parseModule) <* char ')')
            )) <|>
            (yield Unsafe)
        ))
    )

-- This operation parses the result of the 'Deterministic' analysis of CASS.
parseDeterministic :: String -> Maybe Deterministic
parseDeterministic = parse (
        (word "deterministic" *> yield Det) <|>
        (word "non-deterministic" *> yield NDet)
    )

-- This operation parses the result of the 'Demand' analysis of CASS.
parseDemandness :: String -> Maybe Demandness
parseDemandness = parse (
        (word "no demanded arguments" *> yield []) <|>
        (word "demanded arguments: " *> parseList comma parseNumber)
    )

-- This operation parses the result of the 'Indeterministic' analysis of CASS.
parseIndeterministic :: String -> Maybe Indeterministic
parseIndeterministic = parse (
        (word "impure (indeterminstic) operation" *> yield True) <|>
        (word "referentially transparent operation" *> yield False)
    )

-- This operation parses the result of the 'SolComplete' analysis of CASS.
parseSolutionCompleteness :: String -> Maybe SolutionCompleteness
parseSolutionCompleteness = parse (
        (word "solution complete" *> yield True) <|>
        (word "maybe suspend" *> yield False)
    )

-- This operation parses the result of the 'Terminating' analysis of CASS.
parseTermination :: String -> Maybe Termination
parseTermination = parse (
        (word "terminating" *> yield True) <|>
        (word "possibly non-terminating" *> yield False)
    )

-- This operation parses the result of the 'Total' analysis of CASS.
parseTotallyDefined :: String -> Maybe TotallyDefined
parseTotallyDefined = parse (
        (word "totally defined" *> yield True) <|>
        (word "partially defined" *> yield False)
    )

-- HELPER

-- This parser parses a list using a parser for the entries and a parser for the seperators.
parseList :: Parser a -> Parser b -> Parser [b]
parseList sep entry = ((:) <$> entry <*> many (sep *> entry)) <|> yield []

-- This parser parses an integer number.
parseNumber :: Parser Int
parseNumber = read <$> some (check isDigit anyChar)

-- This parser parses a module name.
parseModule :: Parser Module
parseModule = intercalate "." <$> parseList dot ident

-- This parser parses a single '.'.
dot :: Parser ()
dot = char '.'

-- This parser parses a single ','.
comma :: Parser ()
comma = char ','

-- This parser parses a single whitespace.
space :: Parser ()
space = char ' '

-- This parser parses an identifier.
ident :: Parser String
ident = (:) <$> check isAlpha anyChar <*> many (check condition anyChar)
    where
    condition c = isDigit c || isAlpha c || c == '_' || c == '\''

{-
testPath :: String
testPath = "/home/dennis/tmp/.curryanalysis/checkouts/json-3.0.0"

testModule :: Module
testModule = "JSON.Parser"

testOp :: Operation
testOp = "parseJSON"

testOptions :: Options
testOptions = defaultOptions {optVerb = 4}
-}