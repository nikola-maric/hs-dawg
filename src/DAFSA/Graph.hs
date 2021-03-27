{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

{-# LANGUAGE DeriveAnyClass #-}
module DAFSA.Graph (Graph, fromWords, fromWordsAst, contains) where

import Control.DeepSeq (NFData, force)
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import GHC.Generics (Generic)
import Data.Bits (Bits(unsafeShiftL, (.|.)))
import Control.Monad.ST.Strict (ST, runST)
import qualified DAFSA.Internal.GraphBuilder as Builder
import DAFSA.Internal.GraphBuilder (GraphBuilder(..), NodeType(..), GraphNode(..), ID(..))
import Data.STRef.Strict (readSTRef)
import Data.Coerce (coerce)
import qualified Data.List as L

data Graph = Graph { graphTransitions :: !(M.Map Int Int), graphTerminationNodes :: !IS.IntSet } deriving stock (Eq, Generic)

instance NFData Graph

fromWords :: [String] -> Graph
fromWords wrds = runST $ Builder.fromWords (L.sort wrds) >>= toGraph

fromWordsAst :: [String] -> Graph
fromWordsAst wrds = runST $ Builder.fromWords wrds >>= toGraph

contains :: String -> Graph -> Bool
contains wrd g = go wrd g 0 where
    go :: String -> Graph -> Int -> Bool
    go [] Graph{..} nodeId = {-# SCC "finalGraphContainsLastCase" #-} IS.member nodeId graphTerminationNodes
    go (chr : chrs) Graph{..} nodeId = case {-# SCC "finalGraphContainsMapLookup" #-} M.lookup (toTransition chr nodeId) graphTransitions of
        Nothing -> False
        Just nextJump -> go chrs g nextJump

-- Storing nodeId + character transition in one 64-bit int
-- |_________ chr _________|_________ id _________|
--          32 bits                32 bits
-- These are stored in trucbla as keys, and when lookup fails : we don't have that transition
-- If lookup returns next nodeId, and we use that one + nect char to go to another jump, until we are out of chars
-- If last nodeId we got is in set of final nodes, word is contained
toTransition :: Char -> Int -> Int
toTransition chr nodeId = {-# SCC "toTransition" #-} unsafeShiftL (fromEnum chr) 32 .|. nodeId
{-# INLINABLE toTransition #-}

toGraph :: GraphBuilder s -> ST s Graph
toGraph GraphBuilder{..} = do
    _transitions <- readSTRef transitions
    _nodes <- readSTRef nodes
    let !z = force (coerce (fmap fst (filter (\(_id, n) -> nodeType n == Terminating) (M.assocs _nodes))) :: [Int])
        !m = force (fmap (\((pId, chr), cId) -> (toTransition chr (coerce pId), coerce cId :: Int)) (M.assocs _transitions))
    pure Graph { graphTransitions = M.fromList m, graphTerminationNodes = IS.fromList z }