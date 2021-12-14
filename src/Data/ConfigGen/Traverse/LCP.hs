module Data.ConfigGen.Traverse.LCP
    ( commonPrefix
    ) where

import Data.Maybe (fromJust, isJust)

-- everything here is stolen from here: https://stackoverflow.com/questions/21717646/longest-common-prefix-in-haskell
-- I should write some sort of tree to solve this, but in meantime be aware that this code might work unexpectfully
commonPrefix :: [String] -> String
commonPrefix = map fromJust . takeWhile isJust . map the . transpose'

the :: Eq a => [a] -> Maybe a
the [] = Nothing
the (x:xs)
    | and $ map (== x) xs = Just x
    | otherwise = Nothing

transpose' :: [String] -> [String]
transpose' xs' =
    maybe [] id $ do
        ys <- mapM ht xs'
        return $ (map fst ys) : (transpose' (map snd ys))
  where
    ht []     = Nothing
    ht (x:xs) = Just (x, xs)
