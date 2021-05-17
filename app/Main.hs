{-# LANGUAGE BangPatterns #-}
module Main where

import DAFSA.Graph ( contains, fromWords )

main :: IO ()
main = do
    !contents_small <- readFile "data/words-10000"
    let !textWords = concatMap words (lines contents_small)
        !dafsa = fromWords textWords
    print (all (`contains` dafsa) textWords)


