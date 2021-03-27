{-# LANGUAGE BangPatterns #-}
module Main where

import DAFSA.Graph

main :: IO ()
main = do
    !contents_small <- readFile "data/dict.txt"
    let !textWords = concatMap words (lines contents_small)
        !dafsa = fromWords textWords
    print (all (\wrd -> contains wrd dafsa) textWords)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


