{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified DAFSA.Graph as DAFSA
import DAFSA.Graph (Graph)
import Criterion.Main ( bench, bgroup, env, nf, defaultMain, whnf )
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)
import qualified Data.Set as S
import qualified Text.Set as TS
import Text.Set (TextSet)

main :: IO ()
main = 
    defaultMain [
        -- env setupEnvConstruct $ \ ~(_words, _wordsText) -> bgroup "construction" [
        --       bench "Construction 10.000 hs-dawg"  $ whnf DAFSA.fromWordsAst _words,
        --       bench "Construction 10.000 Set"  $ whnf S.fromAscList _words,
        --       bench "Construction 10.000 TextSet"  $ whnf TS.fromAsc _wordsText
        --     ],
        env setupEnvLookup $ \ ~(graph) -> bgroup "lookup hs-dawg" [
            bench "10.000"  $ whnf (DAFSA.contains "alexandriana") graph
            ],
        env setupEnvLookupSet $ \ ~m -> bgroup "lookup Set" [
            bench "10.000"  $ whnf (S.member "alexandriana") m
            ],
        env setupEnvLookupTextSet $ \ ~(textSet, textToSearch) -> bgroup "lookup TextSet" [
            bench "10.000"  $ whnf (TS.member textToSearch) textSet
            ]
    ]


setupEnvConstruct:: IO ([String], [Text])
setupEnvConstruct = do
  !contents <- readFile "data/words-10000"
  let !_words = sort $ concatMap words (lines contents)
  let !_wordsText = T.pack <$> sort (concatMap words (lines contents))
  return (_words, _wordsText)

setupEnvLookupSet :: IO (S.Set String)
setupEnvLookupSet = do
  !contents <- readFile "data/words-10000"
  let !wrds = sort $ concatMap words (lines contents)
  return (S.fromList wrds)

setupEnvLookup :: IO Graph
setupEnvLookup = do
  !contents_big <- readFile "data/words-10000"
  let !wrds = sort $ concatMap words (lines contents_big)
  return (DAFSA.fromWords wrds)

setupEnvLookupTextSet :: IO (TextSet, Text)
setupEnvLookupTextSet = do
  !contents_big <- readFile "data/words-10000"
  let !wrds = T.pack <$> sort (concatMap words (lines contents_big))
  return (TS.fromAsc wrds, T.pack "alexandriana")