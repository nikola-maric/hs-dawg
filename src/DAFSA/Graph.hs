{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module DAFSA.Graph where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics ( Generic )
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Control.Monad.ST.Strict (runST)
import Control.Monad.ST (ST)
import Data.Bool ( bool )
import Data.Maybe (fromJust, isJust, fromMaybe, catMaybes)
import Debug.Trace ( traceM, trace )
import qualified Data.List as L
import Data.List (foldl')
import Control.Monad (foldM)
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Foldable (for_)
import Data.STRef (modifySTRef')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Foldable (find)
import Control.Monad.Cont (filterM)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)

newtype ID = ID { getID :: Int } 
        deriving stock    (Read, Generic)
        deriving newtype  (Eq, Ord, Enum, Bounded, Num, Real, Integral, NFData)
instance Show ID where
        show ID{..} = show getID

data NodeType = Terminating | NonTerminating deriving (Eq, Ord, Read, Generic)
instance Show NodeType where
        show Terminating = "T"
        show NonTerminating = "NT"

instance NFData NodeType

-- invariants ; nodeId must not be equal to nodeLastChildId
data GraphNode = GraphNode {
        nodeId:: !ID,
        nodeType :: !NodeType,
        nodeChildTransitions :: !(S.Set (Char, ID)),
        nodeLastChild :: Maybe (Char, ID)
        } deriving (Ord, Read, Generic)
instance NFData GraphNode
instance Eq GraphNode where
        gn == gn' = totalEquivalence gn gn'
instance Show GraphNode where
        show GraphNode{..} = "{(" <> show nodeId <> "-" <> show nodeType <> ")[" <> show nodeChildTransitions <> "][" <> show nodeLastChild <> "]}"

-- invariant : each ID in transitions must be contained in nodes
--           : root node must always be present in nodes
data Graph s = Graph { 
                nodes :: !(STRef s (M.Map ID GraphNode)),
                maxId :: !(STRef s Int),
                transitions :: !(STRef s (M.Map (ID, Char) ID)),
                rootNode :: !(STRef s GraphNode)
        } deriving (Eq, Generic)
instance NFData (Graph s)

data FinalGraph = FinalGraph {
      fgTransitions :: !(M.Map (Int, Char) Int),
      fgFinalStates :: IntSet
}

emptyGraph :: FinalGraph
emptyGraph = runST $ initialState >>= toFinalGraph


fromWords :: [String] -> FinalGraph
fromWords wrds = 
        let sortedWords = L.sort wrds
        in runST $ do
        initialGraph <- initialState
        register <- newSTRef (S.empty :: S.Set GraphNode)
        for_ sortedWords $ \word -> do
                !commonPrefix <- walkPrefix word initialGraph
                let !currentSuffix = drop (length commonPrefix) word
                !lastNode <- lastState commonPrefix initialGraph
                whenM (nodeHasChildren lastNode) (replaceOrRegister register initialGraph lastNode >> pure ())
                addSuffix initialGraph lastNode currentSuffix
        rNode <- readSTRef (rootNode initialGraph)
        replaceOrRegister register initialGraph rNode
        toFinalGraph initialGraph

toFinalGraph :: Graph s -> ST s FinalGraph
toFinalGraph Graph{..} = do
        _transitions <- readSTRef transitions
        _nodes <- readSTRef nodes
        let z = coerce (fmap fst (filter (\(_id, n) -> nodeType n == Terminating) (M.assocs _nodes))) :: [Int]
            m = fmap (\((pId, chr), cId) -> ((coerce pId :: Int, chr), coerce cId :: Int)) (M.assocs _transitions)
        pure FinalGraph { fgTransitions = M.fromAscList m, fgFinalStates = IS.fromList z }


childGraphNodes :: GraphNode -> [(Char, ID)]
childGraphNodes n = S.toAscList (nodeChildTransitions n)
        -- _nodes <- readSTRef nodes
        -- pure $ fmap (\((_, chr), cId) -> (chr, cId)) (
        --                 filter (\((pId, _), _) -> pId == nodeId n)
        --                 (M.assocs _transitions))

graphContains :: String -> FinalGraph -> Bool
graphContains w g = case lookupInGraph w g of
        Just _id -> IS.member (coerce _id) (fgFinalStates g)
        Nothing -> False

lookupInGraph :: String -> FinalGraph -> Maybe Int
lookupInGraph w FinalGraph{..} = foldlMT (\q c -> M.lookup (q,c) fgTransitions) 0 w
  where -- Stole the implementation from "Data.Foldable"
        foldlMT f z0 xs = foldr f' return xs z0
          where f' x k z = f z x >>= k
{-# INLINABLE lookupInGraph #-}

initialState :: ST s (Graph s)
initialState = do
        rootNode <- newSTRef (mkNewNode 0)
        nodes <- readSTRef rootNode >>= \rn -> newSTRef (M.singleton (nodeId rn) rn)
        transitions <- newSTRef mempty
        maxId <- newSTRef 0
        pure Graph {..}

addSuffix :: Graph s -> GraphNode -> [Char] -> ST s (Graph s)
addSuffix Graph{..} n [] = do
        let terminationNode = GraphNode { nodeId = nodeId n, nodeType = Terminating, nodeChildTransitions = S.empty, nodeLastChild = Nothing  }
        modifySTRef' nodes (M.insert (nodeId n) terminationNode)
        rNode <- readSTRef rootNode
        whenM (pure (nodeId n == nodeId rNode)) $ do
                modifySTRef' rootNode (const terminationNode {nodeChildTransitions = nodeChildTransitions n} )
        pure Graph { .. }
addSuffix g n (x : xs) = do
        _transitions <- readSTRef (transitions g)
        _nodes <- readSTRef (nodes g)
        _rootNode <- readSTRef (rootNode g)
        case M.lookup (nodeId n, x) _transitions of
                Nothing -> do
                        -- traceM ("Graph did not contain " <> show x <> " transition from " <> show (nodeId n) <> ", adding it")
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

replaceOrRegister :: STRef s (S.Set GraphNode) -> Graph s -> GraphNode -> ST s GraphNode
replaceOrRegister registry g n = do
        !lastChild <- getLastChild n g
        x <- case lastChild of
                        Nothing -> pure n
                        Just child -> replaceOrRegister registry g child
        -- traceM ("replaceOrRegister: about to call swapChildState with " <> show n <> " and " <> show x)
        ifM (pure (nodeId x /= nodeId n)) (swapChildState g registry n x) (pure x)
        -- whenM (nodeHasChildren n) (swapChildState g registry n n)
        -- traceM ("replaceOrRegister: finding last child of " <> show n <> ", its " <> show lastChild)
        -- swapChildState g registry n c'
        

swapChildState :: Graph s -> STRef s (S.Set GraphNode) -> GraphNode -> GraphNode -> ST s GraphNode
swapChildState g registry n childNode = do
        equivalentNode <- findEquivalentFrozenNode childNode registry
        -- traceM ("swapChildState : swapping : " <> show childNode <> ", parent is : " <> show n <> ", equivalentNode is " <> show equivalentNode)
        case equivalentNode of
                Nothing -> do
                        -- traceM ("swapChildState : adding is " <> show childNode <> " to register ")
                        modifySTRef' registry (S.insert childNode)
                        pure n
                Just n' -> do
                        -- traceM ("swapChildState : making " <> show (nodeChildTransitions n) <> " transition from node " <> show (nodeId n) <>" point to : " <> show n')
                        -- traceM ("swapChildState : deleting " <> show childNode <> " from graph")
                        let (oldTransition, newSet) = S.deleteFindMax (nodeChildTransitions n)
                        _reg <- readSTRef registry
                        let changedParentNode = GraphNode {
                                nodeId = nodeId n,
                                nodeType = nodeType n,
                                nodeChildTransitions = S.insert (fst oldTransition, nodeId n') newSet,
                                nodeLastChild = Just (fst oldTransition, nodeId n')
                            }
                        modifySTRef' (nodes g) (M.delete (nodeId childNode)) -- deleting child with equivalent state
                        modifySTRef' (nodes g) (M.insert (nodeId changedParentNode) changedParentNode) -- updating parent node in list of all nodes
                        -- whenM (pure (S.member changedParentNode _reg)) (modifySTRef' registry (S.insert changedParentNode))
                        -- unlessM (pure (S.null (nodeChildTransitions n))) (modifySTRef' (transitions g) (M.insert (nodeId n, S.findMax (nodeChildTransitions n)) (nodeId n')))
                        modifySTRef' (transitions g) (M.insert (nodeId changedParentNode, fst (fromJust (nodeLastChild changedParentNode))) (nodeId n'))
                        -- _n <- readSTRef (nodes g)
                        -- traceM ("swapChildState: nodes are now " <> show _n)
                        -- _reg <- readSTRef registry
                        -- traceM ("swapChildState: registry is now " <> show _reg)
                        pure changedParentNode

findEquivalentFrozenNode :: GraphNode -> STRef s (S.Set GraphNode) -> ST s (Maybe GraphNode)
findEquivalentFrozenNode n registry = do
        -- traceM ("findEquivalentFrozenNode of " <> show n )
        reg <- readSTRef registry
        let index = S.lookupIndex n reg
        case index of
          Nothing -> do
                modifySTRef' registry (S.insert n)
                pure Nothing
          Just i -> pure (Just (S.elemAt i reg))

totalEquivalence :: GraphNode -> GraphNode -> Bool
totalEquivalence n n'= equivalentState n n' && childrenEquivalence n n'

equivalentState :: GraphNode -> GraphNode -> Bool
equivalentState n n' = nodeId n /= nodeId n' && nodeType n == nodeType n'

childrenEquivalence :: GraphNode -> GraphNode -> Bool
childrenEquivalence n n' = do
        let equalSizes = S.size (nodeChildTransitions n) == S.size (nodeChildTransitions n')
            equalTransitions = S.null (S.difference (nodeChildTransitions n) (nodeChildTransitions n'))
         in equalSizes && equalTransitions && (childNodes n == childNodes n')

childNodes :: GraphNode -> [ID]
childNodes n = snd <$> childGraphNodes n

lastState :: [(ID, Char)] -> Graph s -> ST s GraphNode
lastState [] g = readSTRef (rootNode g)
lastState xs Graph{..} = do
        _nodes <- readSTRef nodes
        rNode <- readSTRef rootNode
        pure (M.findWithDefault rNode (fst (last xs)) _nodes)

walkPrefix :: String -> Graph s -> ST s [(ID, Char)]
walkPrefix [] _ = pure []
walkPrefix xs g = do
        rId <- nodeId <$> readSTRef (rootNode g)
        go xs g rId [] where
        go :: String -> Graph s -> ID -> [(ID, Char)] -> ST s [(ID, Char)]
        go [] _ _ acc = pure acc
        go (x : xs') graph@Graph{..} _id acc = do
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

-- TODO: stop using fromJust/maximum
getLastChild :: GraphNode -> Graph s -> ST s (Maybe GraphNode)
getLastChild n Graph{..} = case nodeLastChild n of
                                        Nothing -> pure Nothing 
                                        Just (_, _id) -> do
                                                _nodes <- readSTRef nodes
                                                pure (M.lookup _id _nodes)
        

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = ifM c t (pure ())
{-# INLINE whenM #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t f = c >>= bool f t
{-# INLINE ifM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c f = ifM c (pure ()) f
{-# INLINE unlessM #-}