{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}

import DAFSA.Graph (Graph, fromWords, contains)
import Criterion.Main ( bench, bgroup, env, nf, defaultMain, whnf )
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)

main :: IO ()
main = 
    defaultMain [
        env setupEnvConstructText $ \ ~(_words, _moreWords) -> bgroup "construction hs-dawg" [
            bench "10.000"  $ whnf fromWords _words
            -- , bench "100.000" $ whnf fromWords  _moreWords
            ],
        env setupEnvLookup $ \ ~(graph) -> bgroup "lookup hs-dawg" [
            bench "10.000"  $ whnf (contains "alexandriana") graph
            -- , bench "100.000" $ whnf fromWords  _moreWords
            ]
    ]


setupEnvConstructText :: IO ([String], [String])
setupEnvConstructText = do
  !contents_small <- readFile "data/words-10000"
  !contents_big <- readFile "data/words-100000"
  let !_words = sort $ concatMap words (lines contents_small)
  let !_moreWords = sort $ concatMap words (lines contents_big)
  return (_words, _moreWords)

setupEnvLookup :: IO Graph
setupEnvLookup = do
  !contents_big <- readFile "data/words-10000"
  let !wrds = sort $ concatMap words (lines contents_big)
  return (fromWords wrds)