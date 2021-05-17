{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DAFSA.Internal.GraphBuilder where

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import GHC.Generics ( Generic )
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Control.Monad.ST (ST)
import Data.Maybe (fromJust)
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Foldable (for_)
import Data.STRef (modifySTRef')
import Data.Hashable (Hashable (hashWithSalt))
import DAFSA.Internal.MonadUtil ( whenM, ifM )
import Data.Functor ((<&>), ($>))

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

type Transition = (Char, ID)

data GraphNode = GraphNode {
        nodeId:: !ID,
        nodeType :: !NodeType,
        nodeChildTransitions :: !(S.Set Transition),
        nodeLastChild :: Maybe Transition
        } deriving (Ord, Read, Generic)
instance NFData GraphNode
instance Eq GraphNode where
        gn == gn' = totalEquivalence gn gn'

data GraphNodeIdentifier = GraphNodeIdentifier !NodeType !(S.Set Transition) deriving Eq
instance Hashable GraphNodeIdentifier where
        hashWithSalt s (GraphNodeIdentifier _type _children) = s `hashWithSalt` _type `hashWithSalt` S.toAscList _children       


instance Show GraphNode where
        show GraphNode{..} = "{(" <> show nodeId <> "-" <> show nodeType <> ")[" <> show nodeChildTransitions <> "][" <> show nodeLastChild <> "]}"

data GraphBuilder s = GraphBuilder { 
                nodes :: !(STRef s (M.Map ID GraphNode)),
                maxId :: !(STRef s Int),
                transitions :: !(STRef s (M.Map Transition ID)),
                rootNode :: !(STRef s GraphNode)
        } deriving stock (Eq, Generic)
instance NFData (GraphBuilder s)

graphNodeIdentifier :: GraphNode -> GraphNodeIdentifier
graphNodeIdentifier GraphNode{..} = GraphNodeIdentifier nodeType nodeChildTransitions

build :: [String] -> ST s (GraphBuilder s)
build wrds = do
        initialGraph <- initialState
        register <- newSTRef HM.empty
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
        rootNode <- newSTRef (mkNewNode 1)
        nodes <- readSTRef rootNode >>= \rn -> newSTRef (M.singleton (nodeId rn) rn)
        transitions <- newSTRef mempty
        maxId <- newSTRef 1
        pure GraphBuilder {..}

addSuffix :: GraphBuilder s -> GraphNode -> [Char] -> ST s (GraphBuilder s)
addSuffix GraphBuilder{..} n [] = do
        let !_nodeId = nodeId n
            terminationNode = mkNewTerminatingNode _nodeId
        modifySTRef' nodes (M.insert _nodeId terminationNode)
        rNode <- readSTRef rootNode
        whenM (pure (_nodeId == nodeId rNode)) $ do
                modifySTRef' rootNode (const terminationNode {nodeChildTransitions = nodeChildTransitions n} )
        pure GraphBuilder { .. }
addSuffix g n (x : xs) = do
        _transitions <- readSTRef (transitions g)
        let !_nodeId = nodeId n
        case M.lookup (x, _nodeId) _transitions of
                Nothing -> addNodeToGraph g n x xs 
                Just _id -> do
                        nextNode <- readSTRef (nodes g) <&> fromJust . M.lookup _id
                        addSuffix g nextNode xs

addNodeToGraph :: GraphBuilder s -> GraphNode -> Char -> [Char] -> ST s (GraphBuilder s)
addNodeToGraph g currentNode chrToAdd restOfChrs = do
        _rootNode <- readSTRef (rootNode g)
        oldMaxId <- readSTRef (maxId g)
        let !_nodeId = nodeId currentNode
            !newId = coerce (oldMaxId + 1) :: ID
            !newNode = mkNewNode newId
            !newTransition = (chrToAdd, newId)
            !changedLastNode = GraphNode {
                        nodeId = _nodeId,
                        nodeType = nodeType currentNode,
                        nodeChildTransitions = S.insert newTransition (nodeChildTransitions currentNode),
                        nodeLastChild = Just newTransition
                }
        {-# SCC "addNodeToGraph1" #-} writeSTRef (maxId g) (coerce newId)
        {-# SCC "addNodeToGraph2" #-} modifySTRef' (nodes g) (M.insert _nodeId changedLastNode . M.insert newId newNode)
        {-# SCC "addNodeToGraph3" #-} modifySTRef' (transitions g) (M.insert (chrToAdd, _nodeId) newId)
        {-# SCC "addNodeToGraph4" #-} whenM (pure (_nodeId == nodeId _rootNode)) (writeSTRef (rootNode g) changedLastNode)
        addSuffix g newNode restOfChrs

replaceOrRegister :: STRef s (HM.HashMap GraphNodeIdentifier GraphNode) -> GraphBuilder s -> GraphNode -> ST s GraphNode
replaceOrRegister registry g n = do
        !lastChild <- getLastChild n g
        x <- case lastChild of
                Nothing -> pure n
                Just child -> replaceOrRegister registry g child
        ifM (pure (nodeId x /= nodeId n)) (swapChildState g registry n x) (pure x)       

swapChildState :: GraphBuilder s -> STRef s (HM.HashMap GraphNodeIdentifier GraphNode) -> GraphNode -> GraphNode -> ST s GraphNode
swapChildState g registry n childNode = do
        equivalentNode <- findEquivalentFrozenNode childNode registry
        let !_nodeId = nodeId n
        case equivalentNode of
                Nothing -> modifySTRef' registry (HM.insert (graphNodeIdentifier childNode) childNode) $> n
                Just n' -> do
                        let (oldMaxTransition, newSet) = S.deleteFindMax (nodeChildTransitions n)
                            !_nodeId' = nodeId n'
                            !newTransition = (fst oldMaxTransition, _nodeId')
                            !changedParentNode = GraphNode { nodeId = _nodeId, nodeType = nodeType n, nodeChildTransitions = S.insert newTransition newSet, nodeLastChild = Just newTransition }
                        {-# SCC "swapChildState2" #-} modifySTRef' (nodes g) (M.delete (nodeId childNode)) -- deleting child with equivalent state
                        {-# SCC "swapChildState3" #-} modifySTRef' (nodes g) (M.insert (nodeId changedParentNode) changedParentNode) -- updating parent node in list of all nodes
                        {-# SCC "swapChildState4" #-} modifySTRef' (transitions g) (M.insert (fst newTransition, _nodeId) _nodeId')
                        pure changedParentNode

findEquivalentFrozenNode :: GraphNode -> STRef s (HM.HashMap GraphNodeIdentifier GraphNode) -> ST s (Maybe GraphNode)
findEquivalentFrozenNode n registry = do
        reg <- readSTRef registry
        pure (HM.lookup (graphNodeIdentifier n) reg)

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
                case M.lookup (x, _id) trs of
                        Nothing    -> pure acc
                        Just newId -> go xs' graph newId (acc ++ [(newId, x)])

mkNewNode :: ID -> GraphNode
mkNewNode _id = GraphNode { nodeId = _id, nodeType = NonTerminating, nodeChildTransitions = S.empty, nodeLastChild = Nothing }

mkNewTerminatingNode :: ID -> GraphNode
mkNewTerminatingNode _id = let g = mkNewNode _id in g { nodeType = Terminating }

nodeHasChildren :: GraphNode -> ST s Bool
nodeHasChildren = pure . not . S.null . nodeChildTransitions

getLastChild :: GraphNode -> GraphBuilder s -> ST s (Maybe GraphNode)
getLastChild n GraphBuilder{..} = case nodeLastChild n of
                                        Nothing -> pure Nothing 
                                        Just (_, _id) -> do
                                                _nodes <- readSTRef nodes
                                                pure (M.lookup _id _nodes)