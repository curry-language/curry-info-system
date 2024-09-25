module CurryInfo.Helper where

slice :: Int -> Int -> [a] -> [a]
slice start end l = take (end - start) (drop start l)

safeRead :: Read a => String -> Maybe a
safeRead s = case readsPrec 0 s of
    [(x, "")] -> Just x
    _ -> Nothing