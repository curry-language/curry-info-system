-----------------------------------------------------------------------------------------
--- This modules defines operations to parse data from strings.
-----------------------------------------------------------------------------------------

module CurryInfo.Parser where

import CurryInfo.RequestTypes

import Data.List (init, find, intercalate, intersperse)

import DetParse ( Parser, parse, word, (<|>), (*>), yield, failure, some
                , anyChar, char, check, (<$>), (<*>), many, (<*), (<!>) )
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

-- This parser parses a version number with an optional prerelease string
-- following it.
version :: Parser Version
version = (++) <$> versionNumber <*> (prerelease <!> yield [])

-- This parser parses a prerelease string used in a version.
prerelease :: Parser String
prerelease =
  (:) <$> (char '-' *> yield '-') <*>
    (some (check (\c -> c == '-' || isAlphaNum c) anyChar))

-- HELPER

-- This parser tries to parse using the given parser and yields Nothing
-- if that parser fails.
optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <!> yield Nothing

-- This parser parses a list using a parser for the entries and a parser
-- for the seperators.
parseList :: Parser a -> Parser b -> Parser [b]
parseList sep entry = ((:) <$> entry <*> many (sep *> entry)) <|> yield []

-- This parser parses a version number.
versionNumber :: Parser String
versionNumber =
  concat <$> (intersperse "." <$> (map show <$> parseList dot parseNumber))

-- This parser parses an integer number.
parseNumber :: (Read a, Num a) => Parser a
parseNumber = read <$> some (check isDigit anyChar)

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