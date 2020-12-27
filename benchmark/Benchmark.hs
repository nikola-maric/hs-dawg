{-# LANGUAGE BangPatterns #-}
import DAFSA.Graph
import Criterion.Main ( bench, bgroup, env, whnf, defaultMain )

main :: IO ()
main = 
    defaultMain [
        env setupEnvConstruct $ \ ~(_words, _moreWords) -> bgroup "construction" [
            bench "10.000"  $ whnf graphFromList  _words,
            bench "100.000"  $ whnf graphFromList  _moreWords
            ],
        env setupEnvSearch $ \ ~(_words, _moreWords, smallFst, largeFst) -> bgroup "search" [
            bench "one search, small corpus"  $ whnf (graphContains (_words !! 1)) smallFst,
            bench "one search, large corpus"  $ whnf (graphContains (_moreWords !! 1)) largeFst,
            bench "search all, small corpus"  $ whnf (all (`graphContains` smallFst)) _words,
            bench "search all, large corpus"  $ whnf (all (`graphContains` largeFst)) _moreWords
            ]
    ]

-- Setting up word lists
setupEnvConstruct :: IO ([String], [String])
setupEnvConstruct = do
  !contents_small <- readFile "data/words-10000"
  !contents_big <- readFile "data/words-100000"
  let !_words = concatMap words (lines contents_small)
  let !_moreWords = concatMap words (lines contents_big)
  return (_words, _moreWords)


setupEnvSearch :: IO ([String], [String], Graph , Graph )
setupEnvSearch = do
  !contents_small <- readFile "data/words-10000"
  !contents_big <- readFile "data/words-100000"
  let !_words = concatMap words (lines contents_small)
  let !_moreWords = concatMap words (lines contents_big)
  return (_words, _moreWords, graphFromList _words, graphFromList _moreWords)