module CurryInfo.Helper where

-- This operation returns a slice of the given list, with the first index
-- being inclusive and the second index being exclusive.
slice :: Int -> Int -> [a] -> [a]
slice start end l = take (end - start) (drop start l)

-- This operation tries to read a value from a string,
-- returning Nothing if it fails.
safeRead :: Read a => String -> Maybe a
safeRead s = case readsPrec 0 s of
    [(x, "")] -> Just x
    _ -> Nothing