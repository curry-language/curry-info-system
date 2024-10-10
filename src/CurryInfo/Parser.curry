module CurryInfo.Parser where

import CurryInfo.Types

import Data.List (init, find, intercalate, intersperse)

import DetParse (Parser, parse, word, (<|>), (*>), yield, failure, some, anyChar, char, check, (<$>), (<*>), many, (<*), (<!>))
import Prelude hiding ((<|>), (*>), some, (<$>), (<*>), many, (<*))

-- This operation parses the version constraints of a package.
parseVersionConstraints :: String -> Maybe Disjunction
parseVersionConstraints = parse disjunction

-- This parser parses a list of conjunctions.
disjunction :: Parser Disjunction
disjunction = parseList (word " || ") conjunction

-- This parser parses a list of version constraints.
conjunction :: Parser Conjunction
conjunction = parseList (word ", ") versionConstraint

-- This parser parses a version constraint.
versionConstraint :: Parser VersionConstraint
versionConstraint = comparator <*> (ws *> version)

-- This parser parses a comparator of a version constraint.
comparator :: Parser (Version -> VersionConstraint)
comparator =
    (word "<" *> yield VLt) <|>
    (word "<=" *> yield VLte) <|>
    (word ">" *> yield VGt) <|>
    (word ">=" *> yield VGte) <|>
    (word "=" *> yield VExact) <|>
    (word "~" *> yield VMinCompatible) <|>
    (word "^" *> yield VMajCompatible)

-- This parser parses a version number with an optional prerelease string following it.
version :: Parser Version
version = (++) <$> versionNumber <*> (prerelease <!> yield [])

-- This parser parses a prerelease string used in a version.
prerelease :: Parser String
prerelease = (:) <$> (char '-' *> yield '-') <*> (some (check (\c -> c == '-' || isAlphaNum c) anyChar))

-- This operation parses the result of the 'UnsafeModule' analysis of CASS.
parseUnsafe :: String -> Maybe Unsafe
parseUnsafe = parse (
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
parseDemand :: String -> Maybe Demand
parseDemand = parse (
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
parseSolComplete :: String -> Maybe SolComplete
parseSolComplete = parse (
        (word "solution complete" *> yield True) <|>
        (word "maybe suspend" *> yield False)
    )

-- This operation parses the result of the 'Terminating' analysis of CASS.
parseTerminating :: String -> Maybe Terminating
parseTerminating = parse (
        (word "terminating" *> yield True) <|>
        (word "possibly non-terminating" *> yield False)
    )

-- This operation parses the result of the 'Total' analysis of CASS.
parseTotal :: String -> Maybe Total
parseTotal = parse (
        (word "totally defined" *> yield True) <|>
        (word "partially defined" *> yield False)
    )

-- HELPER

-- This parser tries to parse using the given parser and yields Nothing if that parser fails.
optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <!> yield Nothing

-- This parser parses a list using a parser for the entries and a parser for the seperators.
parseList :: Parser a -> Parser b -> Parser [b]
parseList sep entry = ((:) <$> entry <*> many (sep *> entry)) <|> yield []

-- This parser parses the name of a package.
packageName :: Parser Package
packageName = many (check isAlpha anyChar)

-- This parser parses a version number.
versionNumber :: Parser String
versionNumber = concat <$> (intersperse "." <$> (map show <$> parseList dot parseNumber))

-- This parser parses an integer number.
parseNumber :: (Read a, Num a) => Parser a
parseNumber = read <$> some (check isDigit anyChar)

-- This parser parses a module name.
parseModule :: Parser Module
parseModule = intercalate "." <$> parseList dot ident

-- This parser parses some amount of whitespace. It parses at least one character.
ws :: Parser ()
ws = some (check isSpace anyChar) *> yield ()

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

-- This parser parses a single ':'.
colon :: Parser ()
colon = char ':'

-- This parser parses a single ">=".
ge :: Parser ()
ge = word ">="

-- This parser parses a single '>'.
lt :: Parser ()
lt = char '<'

-- This parser parses a single '\"'.
doubleQuote :: Parser ()
doubleQuote = char '\"'