{-# LANGUAGE BangPatterns #-}
module Main where

import DAFSA.Graph
-- import qualified Data.Text as T
-- import qualified Text.Set as TS

main :: IO ()
main = do
    -- !contents_small <- readFile "data/words-10000"
    -- let !textWords = T.pack <$> concatMap words (lines contents_small)
    --     !dafsa = TS.fromFoldable textWords
    -- print (all (\wrd -> TS.member wrd dafsa) textWords)
    !contents_small <- readFile "data/words-10000"
    let !textWords = concatMap words (lines contents_small)
        !dafsa = fromWords textWords
    print (all (\wrd -> contains wrd dafsa) textWords)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


