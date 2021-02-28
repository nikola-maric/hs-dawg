-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveGeneric #-}
module DAFSA.Node where

-- import GHC.Generics (Generic)
-- import qualified Data.HashMap.Strict as Map
-- import Control.DeepSeq (NFData)
-- import Control.Monad.State.Strict (execState, State, execStateT, MonadIO(liftIO), evalStateT, get,put,  StateT(..))
-- import Control.Monad.Identity
-- import Text.Dot hiding (node)
-- import Data.Char (ord)
-- import Control.Monad.ST (runST)
-- import Data.List (sortOn, sortBy)
-- import Data.Ord (Down(Down))
-- import qualified Data.Bifunctor
-- import DAFSA.Id (IDAllocator, ID, newIDAllocator, freshID)
-- import Control.Monad.ST.Strict


-- data NodeType = NonTerminating | Terminating deriving (Eq, Show, Generic)
-- instance NFData NodeType

-- type Transition = Char

-- data Node = Node ID NodeType (Map.HashMap Transition Node)
--     deriving (Show, Eq, Generic)
-- instance NFData Node

-- nodeContains :: String -> Node -> Bool
-- nodeContains [] (Node _ Terminating _) = True
-- nodeContains [] _ = False
-- nodeContains (c : chrs) (Node _ _ _transitions) = case Map.lookup c _transitions of
--     Nothing -> False
--     Just node -> nodeContains chrs node

-- fromList :: [String] -> Node
-- fromList wrds = runST $ do
--     idAllocator <- newIDAllocator
--     rootNode <- emptyNode idAllocator
--     foldM (flip (addToNode idAllocator)) rootNode wrds

-- addToNode :: IDAllocator s -> String -> Node -> ST s Node
-- addToNode _ [] node = pure (markAsTerminated node)
-- addToNode ids (c : chrs) (Node _id _type _others) = do
--     case Map.lookup c _others of
--         -- node does not contain this character, add it to map and create a node
--         Nothing   -> do
--             newNode <- emptyNode ids >>= addToNode ids chrs
--             pure (Node _id _type (Map.insert c newNode _others))
--         -- node containes this transition, go furher
--         Just node -> do
--             newNode <- addToNode ids chrs node
--             pure (Node _id _type (Map.insert c newNode _others)) --pure (Node _type (Map.insert c (addToNode chrs node) _others))

-- terminatingNode :: IDAllocator s -> ST s Node
-- terminatingNode ids = freshID ids >>= \newId -> pure (Node newId NonTerminating Map.empty)

-- markAsTerminated :: Node -> Node
-- markAsTerminated (Node nodeId _ _others) = Node nodeId Terminating _others

-- emptyNode :: IDAllocator s -> ST s Node
-- emptyNode ids = freshID ids >>= \newId -> pure (Node newId NonTerminating Map.empty)

