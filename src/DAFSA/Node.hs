{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module DAFSA.Node where

import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as Map
import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (execState, State, execStateT, MonadIO(liftIO), evalStateT, get,put,  StateT(..))
import Control.Monad.Identity
import Text.Dot hiding (node)
import Data.Char (ord)
import Control.Monad.ST (runST)
import Data.List (sortOn, sortBy)
import Data.Ord (Down(Down))
import qualified Data.Bifunctor


data NodeType = NonTerminating | Terminating deriving (Eq, Show, Generic)
instance NFData NodeType

type Transition = Char

data Node = Node NodeType (Map.HashMap Transition Node)
    deriving (Show, Eq, Generic)
instance NFData Node

data IdNode = IdNode Int Node
instance Show IdNode where
    show (IdNode _id _) = "(" ++ show _id ++ ")"

nodeContains :: String -> Node -> Bool
nodeContains [] (Node Terminating _) = True
nodeContains [] (Node _ _) = False
nodeContains (c : chrs) (Node _ _transitions) = case Map.lookup c _transitions of
    Nothing -> False
    Just node -> nodeContains chrs node

addToNode :: String -> Node -> Node
addToNode [] node = markAsTerminated node
addToNode (c : chrs) (Node _type _others) = case Map.lookup c _others of
    -- node does not contain this character, add it to map and create a node
    Nothing   -> Node _type (Map.insert c (addToNode chrs emptyNode) _others)
    -- node containes this transition, go furher
    Just node -> Node _type (Map.insert c (addToNode chrs node) _others)


addToNode' :: String -> Node -> Node
addToNode' word node = 
    case lastInsertion of
        [] -> addToNode word node
        transitions -> runST $ do
            return (addToNode word node)
    where lastInsertion = lastWord node
    
    -- return undefined

-- For every node, starting from this one, find MAX transition and concatenate it with those of its children
lastWord :: Node -> [(Transition, Node)]
lastWord (Node _ others) = case Map.null others of
    True -> []
    False -> [lastTransition] <> lastWord (snd lastTransition) where
        lastTransition = head . sortOn (Down . fst) $ Map.toList others

terminatingNode :: Node
terminatingNode = Node Terminating Map.empty

markAsTerminated :: Node -> Node
markAsTerminated (Node _ _others) = Node Terminating _others

emptyNode :: Node
emptyNode = Node NonTerminating Map.empty

nodeFromList :: [String] -> Node
nodeFromList = foldr addToNode emptyNode

