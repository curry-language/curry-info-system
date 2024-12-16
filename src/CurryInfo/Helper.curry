-----------------------------------------------------------------------------------------
--- This modules defines operations that are helpful in several cases.
-----------------------------------------------------------------------------------------

module CurryInfo.Helper where

import Data.Char ( isAlpha, isAlphaNum )
import System.IO

import JSON.Data

-- Reads a sequence of lines from a file, with the start index
-- being inclusive and the stop index being exclusive.
readSliceFromFile :: FilePath -> Int -> Int -> IO String
readSliceFromFile fname start end = do
   h <- openFile fname ReadMode
   mapM_ (\_ -> hGetLine h) [1..start]
   slicelines <- mapM (\_ -> hGetLine h) [1 .. (end-start)]
   hClose h
   return $ unlines slicelines

-- This operation tries to read a value from a string,
-- returning Nothing if it fails.
safeRead :: Read a => String -> Maybe a
safeRead s = case readsPrec 0 s of
    [(x, "")] -> Just x
    _         -> Nothing

--- This type is used to differentiate between a request failing because of
--- missing information and other errors.
data InformationResult =
    InformationError String
  | InformationResult JValue String
  | InformationExtractionFailed
  deriving Eq

--- This operation returns the result of the matching functions of the
--- three given ones.
--- The first function is for the constructor `InformationExtractionFailed`.
--- The second function is for the constructor `InformationError`.
--- The third function is for the constructor `InformationResult`.
information :: a -> (String -> a) -> (JValue -> String -> a)
            -> InformationResult -> a
information ext _ _ InformationExtractionFailed = ext
information _ err _ (InformationError s) = err s
information _ _ res (InformationResult jv s) = res jv s

--- This operation parenthesizes the given string.
parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

-- Is a string a name of an allowed Curry identifier (i.e., not a generated id)?
isCurryID :: String -> Bool
isCurryID n = case n of
  []               -> False
  x:xs | isAlpha x -> all (\c -> isAlphaNum c || c `elem` "'_") xs
       | otherwise -> all (flip elem opChars) n
 where
  opChars = "~!@#$%^&*+-=<>?./|\\:"
