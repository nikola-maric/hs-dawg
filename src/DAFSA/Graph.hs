{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveAnyClass #-}

module DAFSA.Graph (Graph, fromWords, fromWordsAst, contains)  where

import Control.DeepSeq (NFData, force)
import qualified Data.Map.Strict as M

import qualified Data.IntSet as IS
import GHC.Generics (Generic)
import Data.Bits (Bits(unsafeShiftL, (.|.), clearBit, setBit, testBit))
import Control.Monad.ST.Strict (ST, runST)
import qualified DAFSA.Internal.GraphBuilder as Builder
import DAFSA.Internal.GraphBuilder (GraphBuilder(..), NodeType(..), GraphNode(..), ID(..))
import Data.STRef.Strict (readSTRef)
import Data.Coerce (coerce)
import qualified Data.List as L

data Graph = Graph { graphTransitions :: !(M.Map Int Int), graphRootNode :: !Int } deriving stock (Eq, Show, Generic)

instance NFData Graph

fromWords :: [String] -> Graph
fromWords wrds = runST $ Builder.build (L.sort wrds) >>= toGraph

fromWordsAst :: [String] -> Graph
fromWordsAst wrds = runST $ Builder.build wrds >>= toGraph

contains :: String -> Graph -> Bool
contains w g = go w (Just (graphRootNode g)) where
                go :: String -> Maybe Int -> Bool
                go _ Nothing = False 
                go [] (Just node) = testBit node 31
                go (chr : chrs) (Just node) = go chrs (handleTransition g node chr)

handleTransition :: Graph -> Int -> Char -> Maybe Int
handleTransition g currentId c = M.lookup (toTransition c currentId) (graphTransitions g)   
{-# INLINABLE handleTransition #-}

-- Storing character transition + termination info + nodeId in one 64-bit int
-- |_________ chr _________|_termination bit_|_______ id _________|
--          32 bits              1 bit              31 bits
toTransition :: Char -> Int -> Int
toTransition chr nodeId = unsafeShiftL (fromEnum chr) 32 .|. nodeId
{-# INLINABLE toTransition #-}

toGraph :: GraphBuilder s -> ST s Graph
toGraph GraphBuilder{..} = do
    _transitions <- readSTRef transitions
    _nodes <- readSTRef nodes
    rootNodeId <- nodeId <$> readSTRef rootNode
    let !terminations = IS.fromList (coerce (fmap fst (filter (\(_id, n) -> nodeType n == Terminating) (M.assocs _nodes))) :: [Int])
        !m = force (fmap (\((chr, pId), cId) -> (toTransition chr (packNodeId pId terminations), packNodeId cId terminations)) (M.assocs _transitions))
    pure Graph { graphTransitions = M.fromList m, graphRootNode = packNodeId rootNodeId terminations}


packNodeId :: ID -> IS.IntSet -> Int
packNodeId _id _terminations = if IS.member intId _terminations
                                then setBit intId 31     -- place 1 at 32nd bit of number, indicating termination
                                else clearBit intId 31   -- place 0 at 32nd bit of number, indicating non-termination  
                               where intId = coerce _id


