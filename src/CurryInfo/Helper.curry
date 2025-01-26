------------------------------------------------------------------------------
--- This modules defines operations that are helpful in several cases.
------------------------------------------------------------------------------

module CurryInfo.Helper where

import Data.Char ( isAlpha, isAlphaNum )
import Data.List ( splitOn )
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

--- This type represents a result of a request computed for some object.
--- It differentiates between a regular result, an (yet) unknown request,
--- and some other error.
data RequestResult =
    RequestResult JValue String
  | RequestUnknown
  | RequestError String
  deriving Eq

--- This operation transforms an `RequestResult` into some other type
--- according to mappings for the three kinds of `RequestResult` constructors.
--- The first argument defines the result for `RequestUnknown`.
--- The second argument maps the constructor `RequestError` to a result.
--- The third argument maps the constructor `RequestResult` to a result.
fromRequestResult :: a -> (String -> a) -> (JValue -> String -> a)
                      -> RequestResult -> a
fromRequestResult ext _ _ RequestUnknown       = ext
fromRequestResult _ err _ (RequestError s)     = err s
fromRequestResult _ _ res (RequestResult jv s) = res jv s

--- This operation puts single quotation marks around the given string.
quote :: String -> String
quote s = "'" ++ s ++ "'"

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

-- Is a string a (non-hierarchical) module identifier?
isModuleID :: String -> Bool
isModuleID []     = False
isModuleID (x:xs) = isAlpha x && all (\c -> isAlphaNum c || c `elem` "'_") xs

--- Transforms a possible qualified name into a pair of a module name
--- (which might be empty) and an unqualified name.
fromQName :: String -> (String,String)
fromQName = fromQN ""
 where
  fromQN mp s =
    let (m,dotn) = break (=='.') s
    in if null dotn || not (isModuleID m)
         then (mp,s)
         else if null mp then fromQN m (tail dotn)
                         else fromQN (mp ++ '.':m) (tail dotn)
