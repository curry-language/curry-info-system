-----------------------------------------------------------------------------------------
--- This modules defines operations that are helpful in several cases.
-----------------------------------------------------------------------------------------

module CurryInfo.Helper where

import JSON.Data

-- This operation returns a slice of the given list, with the first index
-- being inclusive and the second index being exclusive.
slice :: Int -> Int -> [a] -> [a]
slice start end l = take (end - start) (drop start l)

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
