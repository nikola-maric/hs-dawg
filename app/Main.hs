{-# LANGUAGE BangPatterns #-}
module Main where

import DAFSA.Graph

main :: IO ()
main = do
    !contents_small <- readFile "data/words-100000"
    let !textWords = concatMap words (lines contents_small)
        !dafsa = fromWords textWords
    print (graphContains "alexandriana" dafsa)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


