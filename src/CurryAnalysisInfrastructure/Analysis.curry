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

analyseSafeModule :: Options -> String -> Module -> IO (Maybe Safe)
analyseSafeModule opts path m = do
    analyse opts path "UnsafeModule" m m parseSafe

analyseDeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Deterministic)
analyseDeterministic opts path m op = do
    analyse opts path "Deterministic" m op parseDeterministic

analyseDemandness :: Options -> String -> Module -> Operation -> IO (Maybe Demandness)
analyseDemandness opts path m op = do
    analyse opts path "Demand" m op parseDemandness

analyseIndeterministic :: Options -> String -> Module -> Operation -> IO (Maybe Indeterministic)
analyseIndeterministic opts path m op = do
    analyse opts path "Indeterministic" m op parseIndeterministic

analyseSolutionCompleteness :: Options -> String -> Module -> Operation -> IO (Maybe SolutionCompleteness)
analyseSolutionCompleteness opts path m op = do
    analyse opts path "SolComplete" m op parseSolutionCompleteness

analyseTermination :: Options -> String -> Module -> Operation -> IO (Maybe Termination)
analyseTermination opts path m op = do
    analyse opts path "Terminating" m op parseTermination

analyseTotallyDefined :: Options -> String -> Module -> Operation -> IO (Maybe TotallyDefined)
analyseTotallyDefined opts path m op = do
    analyse opts path "Total" m op parseTotallyDefined
{-
operationConfiguration :: Configuration CurryOperation OperationInformation
operationConfiguration =
    [ ("Operation", (extractOperationOperation, generateOperationOperation))
    , ("Documentation", (extractOperationDocumentation, generateOperationDocumentation))
    , ("SourceCode", (extractOperationSourceCode, generateOperationSourceCode))
    , ("Signature", (extractOperationSignature, generateOperationSignature))
    , ("Infix", (extractOperationInfix, generateOperationInfix))
    , ("Precedence", (extractOperationPrecedence, generateOperationPrecedence))
    , ("Determinism", (extractOperationDeterminism, generateOperationDeterminism))
    , ("Demandness", (extractOperationDemandness, generateOperationDemandness))
    , ("Indeterminism", (extractOperationIndeterminism, generateOperationIndeterminism))
    , ("SolutionCompleteness", (extractOperationSolutionCompleteness, generateOperationSolutionCompleteness))
    , ("Termination", (extractOperationTermination, generateOperationTermination))
    , ("TotallyDefined", (extractOperationTotallyDefined, generateOperationTotallyDefined))
    ]
-}
-- PARSER

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

parseDeterministic :: String -> Maybe Deterministic
parseDeterministic = parse (
        (word "deterministic" *> yield Det) <|>
        (word "non-deterministic" *> yield NDet)
    )

parseDemandness :: String -> Maybe Demandness
parseDemandness = parse (
        (word "no demanded arguments" *> yield []) <|>
        (word "demanded arguments: " *> parseList comma parseNumber)
    )

parseIndeterministic :: String -> Maybe Indeterministic
parseIndeterministic = parse (
        (word "impure (indeterminstic) operation" *> yield True) <|>
        (word "referentially transparent operation" *> yield False)
    )

parseSolutionCompleteness :: String -> Maybe SolutionCompleteness
parseSolutionCompleteness = parse (
        (word "solution complete" *> yield True) <|>
        (word "maybe suspend" *> yield False)
    )

parseTermination :: String -> Maybe Termination
parseTermination = parse (
        (word "terminating" *> yield True) <|>
        (word "possibly non-terminating" *> yield False)
    )

parseTotallyDefined :: String -> Maybe TotallyDefined
parseTotallyDefined = parse (
        (word "totally defined" *> yield True) <|>
        (word "partially defined" *> yield False)
    )

parseList :: Parser a -> Parser b -> Parser [b]
parseList sep entry = ((:) <$> entry <*> many (sep *> entry)) <|> yield []

parseNumber :: Parser Int
parseNumber = read <$> some (check isDigit anyChar)

parseModule :: Parser Module
parseModule = intercalate "." <$> parseList dot ident

dot :: Parser ()
dot = char '.'

comma :: Parser ()
comma = char ','

space :: Parser ()
space = char ' '

ident :: Parser String
ident = (:) <$> check isAlpha anyChar <*> many (check condition anyChar)
    where
    condition c = isDigit c || isAlpha c || c == '_' || c == '\''

testPath :: String
testPath = "/home/dennis/tmp/.curryanalysis/checkouts/json-3.0.0"

testModule :: Module
testModule = "JSON.Parser"

testOp :: Operation
testOp = "parseJSON"

testOptions :: Options
testOptions = defaultOptions {optVerb = 4}
