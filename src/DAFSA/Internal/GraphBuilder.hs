{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DAFSA.Internal.GraphBuilder where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics ( Generic )
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Control.Monad.ST (ST)
import Data.Maybe (fromJust)
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Foldable (for_)
import Data.STRef (modifySTRef')
import qualified Data.HashTable.Class    as HT
import qualified Data.HashTable.ST.Basic as STH
import Data.Hashable (Hashable (hashWithSalt))
import DAFSA.MonadUtil ( whenM, ifM )

newtype ID = ID { getID :: Int } 
        deriving stock    (Read, Generic)
        deriving newtype  (Eq, Ord, Enum, Bounded, Num, Real, Integral, NFData, Hashable)
instance Show ID where
        show ID{..} = show getID

data NodeType = Terminating | NonTerminating deriving stock (Eq, Ord, Read, Generic)
instance Hashable NodeType
instance Show NodeType where
        show Terminating = "T"
        show NonTerminating = "NT"
instance NFData NodeType

data GraphNode = GraphNode {
        nodeId:: !ID,
        nodeType :: !NodeType,
        nodeChildTransitions :: !(S.Set (Char, ID)),
        nodeLastChild :: Maybe (Char, ID)
        } deriving (Ord, Read, Generic)
instance NFData GraphNode
instance Eq GraphNode where
        gn == gn' = totalEquivalence gn gn'

data GraphNodeIdentifier = GraphNodeIdentifier !NodeType !(S.Set (Char, ID)) deriving Eq
instance Hashable GraphNodeIdentifier where
        hashWithSalt s (GraphNodeIdentifier _type _children) = s `hashWithSalt` _type `hashWithSalt` S.toAscList _children       


instance Show GraphNode where
        show GraphNode{..} = "{(" <> show nodeId <> "-" <> show nodeType <> ")[" <> show nodeChildTransitions <> "][" <> show nodeLastChild <> "]}"


data GraphBuilder s = GraphBuilder { 
                nodes :: !(STRef s (M.Map ID GraphNode)),
                maxId :: !(STRef s Int),
                transitions :: !(STRef s (M.Map (ID, Char) ID)),
                rootNode :: !(STRef s GraphNode)
        } deriving (Eq, Generic)
instance NFData (GraphBuilder s)

graphNodeIdentifier :: GraphNode -> GraphNodeIdentifier
graphNodeIdentifier GraphNode{..} = GraphNodeIdentifier nodeType nodeChildTransitions

fromWords :: [String] -> ST s (GraphBuilder s)
fromWords wrds = do
        initialGraph <- initialState
        register <- newSTRef =<< HT.new
        for_ wrds $ \word -> do
                !commonPrefix <- walkPrefix word initialGraph
                let !currentSuffix = drop (length commonPrefix) word
                !lastNode <- lastState commonPrefix initialGraph
                whenM (nodeHasChildren lastNode) (replaceOrRegister register initialGraph lastNode >> pure ())
                addSuffix initialGraph lastNode currentSuffix
        rNode <- readSTRef (rootNode initialGraph)
        _ <- replaceOrRegister register initialGraph rNode
        pure initialGraph

childGraphNodes :: GraphNode -> [(Char, ID)]
childGraphNodes n = S.toAscList (nodeChildTransitions n)

initialState :: ST s (GraphBuilder s)
initialState = do
        rootNode <- newSTRef (mkNewNode 0)
        nodes <- readSTRef rootNode >>= \rn -> newSTRef (M.singleton (nodeId rn) rn)
        transitions <- newSTRef mempty
        maxId <- newSTRef 0
        pure GraphBuilder {..}

addSuffix :: GraphBuilder s -> GraphNode -> [Char] -> ST s (GraphBuilder s)
addSuffix GraphBuilder{..} n [] = do
        let terminationNode = GraphNode { nodeId = nodeId n, nodeType = Terminating, nodeChildTransitions = S.empty, nodeLastChild = Nothing  }
        modifySTRef' nodes (M.insert (nodeId n) terminationNode)
        rNode <- readSTRef rootNode
        whenM (pure (nodeId n == nodeId rNode)) $ do
                modifySTRef' rootNode (const terminationNode {nodeChildTransitions = nodeChildTransitions n} )
        pure GraphBuilder { .. }
addSuffix g n (x : xs) = do
        _transitions <- readSTRef (transitions g)
        _nodes <- readSTRef (nodes g)
        _rootNode <- readSTRef (rootNode g)
        case M.lookup (nodeId n, x) _transitions of
                Nothing -> do
                        oldMaxId <- readSTRef (maxId g)
                        let newId = oldMaxId + 1
                            newNode = mkNewNode (coerce newId)
                            changedLastNode = GraphNode {
                                nodeId = nodeId n,
                                nodeType = nodeType n,
                                nodeChildTransitions = S.insert (x, coerce newId) (nodeChildTransitions n),
                                nodeLastChild = Just (x, coerce newId)
                            }
                        writeSTRef (maxId g) newId
                        modifySTRef' (nodes g) (M.insert (nodeId n) changedLastNode . M.insert (coerce newId) newNode)
                        modifySTRef' (transitions g) (M.insert (nodeId n, x) (coerce newId))
                        whenM (pure (nodeId n == nodeId _rootNode)) (modifySTRef' (rootNode g) (\r@GraphNode{..} -> r { nodeChildTransitions = S.insert (x, coerce newId) nodeChildTransitions, nodeLastChild = Just (x, coerce newId) }))
                        addSuffix g newNode xs
                Just _id -> addSuffix g (fromJust (M.lookup _id _nodes)) xs

replaceOrRegister :: STRef s (STH.HashTable s GraphNodeIdentifier GraphNode) -> GraphBuilder s -> GraphNode -> ST s GraphNode
replaceOrRegister registry g n = do
        !lastChild <- getLastChild n g
        x <- case lastChild of
                        Nothing -> pure n
                        Just child -> replaceOrRegister registry g child
        ifM (pure (nodeId x /= nodeId n)) (swapChildState g registry n x) (pure x)       

swapChildState :: GraphBuilder s -> STRef s (STH.HashTable s GraphNodeIdentifier GraphNode) -> GraphNode -> GraphNode -> ST s GraphNode
swapChildState g registry n childNode = do
        equivalentNode <- findEquivalentFrozenNode childNode registry
        reg <- readSTRef registry
        case equivalentNode of
                Nothing -> do
                        HT.insert reg (graphNodeIdentifier childNode) childNode
                        pure n
                Just n' -> do
                        let (oldTransition, newSet) = S.deleteFindMax (nodeChildTransitions n)
                        let changedParentNode = GraphNode {
                                nodeId = nodeId n,
                                nodeType = nodeType n,
                                nodeChildTransitions = S.insert (fst oldTransition, nodeId n') newSet,
                                nodeLastChild = Just (fst oldTransition, nodeId n')
                            }
                        modifySTRef' (nodes g) (M.delete (nodeId childNode)) -- deleting child with equivalent state
                        modifySTRef' (nodes g) (M.insert (nodeId changedParentNode) changedParentNode) -- updating parent node in list of all nodes
                        modifySTRef' (transitions g) (M.insert (nodeId changedParentNode, fst (fromJust (nodeLastChild changedParentNode))) (nodeId n'))
                        pure changedParentNode

findEquivalentFrozenNode :: GraphNode -> STRef s (STH.HashTable s GraphNodeIdentifier GraphNode) -> ST s (Maybe GraphNode)
findEquivalentFrozenNode n registry = do
        reg <- readSTRef registry
        same <- HT.lookup reg (graphNodeIdentifier n)
        case same of
          Nothing -> do
                pure Nothing
          Just x -> pure (Just x)

totalEquivalence :: GraphNode -> GraphNode -> Bool
totalEquivalence n n'= equivalentState n n' && childrenEquivalence n n'

equivalentState :: GraphNode -> GraphNode -> Bool
equivalentState n n' = nodeType n == nodeType n'

childrenEquivalence :: GraphNode -> GraphNode -> Bool
childrenEquivalence n n' = do
        let equalSizes = S.size (nodeChildTransitions n) == S.size (nodeChildTransitions n')
            equalTransitions = S.null (S.difference (nodeChildTransitions n) (nodeChildTransitions n'))
         in equalSizes && equalTransitions && (childNodes n == childNodes n')

childNodes :: GraphNode -> [ID]
childNodes n = snd <$> childGraphNodes n

lastState :: [(ID, Char)] -> GraphBuilder s -> ST s GraphNode
lastState [] g = readSTRef (rootNode g)
lastState xs GraphBuilder{..} = do
        _nodes <- readSTRef nodes
        rNode <- readSTRef rootNode
        pure (M.findWithDefault rNode (fst (last xs)) _nodes)

walkPrefix :: String -> GraphBuilder s -> ST s [(ID, Char)]
walkPrefix [] _ = pure []
walkPrefix xs g = do
        rId <- nodeId <$> readSTRef (rootNode g)
        go xs g rId [] where
        go :: String -> GraphBuilder s -> ID -> [(ID, Char)] -> ST s [(ID, Char)]
        go [] _ _ acc = pure acc
        go (x : xs') graph@GraphBuilder{..} _id acc = do
                trs <- readSTRef transitions
                case M.lookup (_id, x) trs of
                        Nothing    -> pure acc
                        Just newId -> go xs' graph newId (acc ++ [(newId, x)])

mkNewNode :: Int -> GraphNode
mkNewNode _id = GraphNode {
             nodeId = coerce _id,
             nodeType = NonTerminating,
             nodeChildTransitions = S.empty,
             nodeLastChild = Nothing
        }

nodeHasChildren :: GraphNode -> ST s Bool
nodeHasChildren = pure . not . S.null . nodeChildTransitions

getLastChild :: GraphNode -> GraphBuilder s -> ST s (Maybe GraphNode)
getLastChild n GraphBuilder{..} = case nodeLastChild n of
                                        Nothing -> pure Nothing 
                                        Just (_, _id) -> do
                                                _nodes <- readSTRef nodes
                                                pure (M.lookup _id _nodes)