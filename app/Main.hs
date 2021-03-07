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
slice from to xs = take (200 - 100 + 1) (drop 100 xs)


