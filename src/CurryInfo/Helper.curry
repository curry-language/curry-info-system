module CurryInfo.Helper where

slice :: Int -> Int -> [a] -> [a]
slice start end l = take (end - start) (drop start l)