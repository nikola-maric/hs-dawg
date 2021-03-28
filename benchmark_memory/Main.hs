{-# LANGUAGE BangPatterns #-}
module Main where

import Weigh
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)
import qualified Data.Set as S
import qualified Text.Set as TS
import qualified DAFSA.Graph as DAFSA

main :: IO ()
main = do
  contents <- setupEnv
  mainWith $ do
    consctructionBenchmark contents

consctructionBenchmark :: ([String], [Text]) -> Weigh ()
consctructionBenchmark (strings, texts) =
  wgroup "Construction Benchmark" $ do
    setColumns [Case, Allocated, Max, Live, GCs, MaxOS]
    func' "hs-dawg" DAFSA.fromWordsAst strings
    func' "Set" S.fromAscList strings
    func' "TextSet" TS.fromAsc texts


setupEnv:: IO ([String], [Text])
setupEnv = do
  !contents <- readFile "data/words-10000"
  let !_words = sort $ concatMap words (lines contents)
  let !_wordsText = T.pack <$> sort (concatMap words (lines contents))
  return (_words, _wordsText)