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
        nodeChildTransitions :: !(S.Set Char) -- replace with max heap
        } deriving (Eq, Ord, Read, Generic)
instance NFData GraphNode
instance Show GraphNode where
        show GraphNode{..} = "{(" <> show nodeId <> "-" <> show nodeType <> ")[" <> show nodeChildTransitions <> "]}"

-- invariant : each ID in transitions must be contained in nodes
--           : root node must always be present in nodes
data Graph = Graph { 
                nodes :: !(M.Map ID GraphNode),
                transitions :: !(M.Map (ID, Char) ID),
                register :: !(S.Set GraphNode),
                rootNode :: !GraphNode
        } deriving (Eq, Ord, Show, Read, Generic)
instance NFData Graph

fromWords :: [String] -> Graph
fromWords wrds = runST $ do
        let graph = foldl' (flip addWord) emptyGraph (L.sort wrds)
        replaceOrRegisterRoot graph (rootNode graph)

childGraphNodes :: GraphNode -> Graph -> [(Char, GraphNode)]
childGraphNodes n Graph{..} = (\chr -> (chr, fromJust (M.lookup (fromJust (M.lookup (nodeId n, chr) transitions)) nodes))) <$> S.toList (nodeChildTransitions n)

-- TODO: stop using fromJust
graphContains :: String -> Graph -> Bool
graphContains w g = go w (rootNode g) where
        go :: String -> GraphNode -> Bool
        go [] n = nodeType n == Terminating
        go (x : xs) n = case M.lookup (nodeId n, x) (transitions g) of
                Nothing -> False
                Just nextId -> go xs (fromJust (M.lookup nextId (nodes g)))

emptyGraph :: Graph
emptyGraph = let rNode = mkNewNode 0
             in Graph { nodes = M.singleton (nodeId rNode) rNode, transitions = mempty, rootNode = rNode, register = S.empty  }

addWord :: String -> Graph -> Graph
addWord word g = runST $ do
        -- traceM ("addWord " <> word) 
        let !commonPrefix = walkPrefix word g (nodeId (rootNode g))
            !currentSuffix = drop (length commonPrefix) word
            !lastNode = lastState commonPrefix g
        -- traceM ("inserting " <> show word <> ", common prefix is " <> show commonPrefix <> ", current suffix is " <> show currentSuffix <> ", last node is " <> show lastNode)
        !g' <- ifM (pure (nodeHasChildren lastNode)) (replaceOrRegister g lastNode) (pure g)
        -- traceM ("adding suffix : " <> show currentSuffix)
        !g'' <- addSuffix g' lastNode currentSuffix
        -- traceM ("graph is now : " <> show g'')
        pure g''

addSuffix :: Graph -> GraphNode -> [Char] -> ST s Graph
addSuffix g n [] = do
        let terminationNode = GraphNode { nodeId = nodeId n, nodeType = Terminating, nodeChildTransitions = S.empty  }
        pure Graph {
                nodes = M.insert (nodeId n) terminationNode (nodes g),
                transitions = transitions g,
                register = register g,
                rootNode = if nodeId n == nodeId (rootNode g)
                                then GraphNode { nodeId = nodeId n, nodeType = Terminating,  nodeChildTransitions = nodeChildTransitions n }
                                else rootNode g
        }
addSuffix g n (x : xs) = do
        case M.lookup (nodeId n, x) (transitions g) of
                Nothing -> do
                        let newId = maximum (M.keys (nodes g)) + 1
                            newNode = mkNewNode (coerce newId)
                            changedLastNode = GraphNode {
                                nodeId = nodeId n,
                                nodeType = nodeType n,
                                nodeChildTransitions = S.insert x (nodeChildTransitions n)
                            }
                            newGraph = Graph {
                                nodes = M.insert (nodeId n) changedLastNode (M.insert newId newNode (nodes g)),
                                register = register g,
                                transitions = M.insert (nodeId n, x) newId (transitions g),
                                rootNode = if nodeId n == nodeId (rootNode g)
                                                then GraphNode { nodeId = nodeId n, nodeType = nodeType n,  nodeChildTransitions = S.insert x (nodeChildTransitions n) }
                                                else rootNode g
                            }
                         in addSuffix newGraph newNode xs
                Just _id -> addSuffix g (fromJust (M.lookup _id (nodes g))) xs

replaceOrRegister :: Graph -> GraphNode -> ST s Graph
replaceOrRegister g n = do
        let !lastChild = getLastChild n g
        -- traceM ("replaceOrRegister: finding last child of " <> show n <> ", its " <> show lastChild)
        !g' <- ifM (pure (nodeHasChildren lastChild)) (replaceOrRegister g lastChild) (pure g)
        !g'' <- ifM (graphContainsFrozenState lastChild g') (swapChildState g' n lastChild) (pure g')
        pure g'' { register = S.insert lastChild (register g'') }
        -- traceM ("replaceOrRegister: last child is " <> show lastChild)

replaceOrRegisterRoot :: Graph -> GraphNode -> ST s Graph
replaceOrRegisterRoot g n = ifM (pure (nodeHasChildren n)) (replaceOrRegister g n) (pure g)

swapChildState :: Graph -> GraphNode -> GraphNode -> ST s Graph
swapChildState g n childNode = do
        let !equivalentNode = findEquivalentFrozenNode childNode g
        -- traceM ("swapChildState : swapping : " <> show childNode <>  ", equivalentNode is " <> show equivalentNode)
        case equivalentNode of
                Nothing -> do
                        -- traceM ("swapChildState : adding is " <> show childNode <> " to register ")
                        pure g { register = S.insert childNode (register g)}
                Just n' -> do
                        -- traceM ("swapChildState : making " <> show (S.findMax (nodeChildTransitions n)) <> " transition from node " <> show (nodeId n) <>" point to : " <> show n')
                        -- traceM ("swapChildState : deleting " <> show childNode <> " from graph")
                        pure g {
                        transitions = M.insert (nodeId n, S.findMax (nodeChildTransitions n)) (nodeId n') (transitions g),
                        nodes = M.delete (nodeId childNode) (nodes g)
                }

findEquivalentFrozenNode :: GraphNode -> Graph -> Maybe GraphNode
findEquivalentFrozenNode n g = do
        -- x <- L.find (\n' -> nodeId n /= nodeId n' 
        --                                 && nodeChildTransitions n == nodeChildTransitions n'
        --                                 && nodeType n == nodeType n'
        --                         )
        --                         (register g)
        x <- L.find (\n' -> totalEquivalence n n' g)(register g)
        -- traceM ("findEquivalentFrozenNode : equivalent of " <> show (nodeId n) <> " is " <> show x)
        pure x

totalEquivalence :: GraphNode -> GraphNode -> Graph -> Bool
totalEquivalence n n' g = equivalentState n n' && childrenEquivalence n n' g

equivalentState :: GraphNode -> GraphNode -> Bool
equivalentState n n' = nodeId n /= nodeId n' && nodeType n == nodeType n'

childrenEquivalence :: GraphNode -> GraphNode -> Graph -> Bool
childrenEquivalence n n' g = S.size (nodeChildTransitions n) == S.size (nodeChildTransitions n') &&
        nodeChildTransitions n == nodeChildTransitions n' && 
        fmap nodeId (childNodes n g) == fmap nodeId (childNodes n' g)

-- zipChildren :: GraphNode -> GraphNode -> Graph -> [(GraphNode, GraphNode)]
-- zipChildren n n' g = zip (childNodes n g) (childNodes n' g)

childNodes :: GraphNode -> Graph -> [GraphNode]
childNodes n g = fmap snd (childGraphNodes n g)


-- If graph contains node with exactly same type and transitions, that is not this particular node
graphContainsFrozenState :: GraphNode -> Graph -> ST s Bool
graphContainsFrozenState n g = do 
        -- traceM ("graphContainsFrozenState " <> show n <> " : " <> show (findEquivalentFrozenNode n g) <> ", register is " <> show (register g))
        case findEquivalentFrozenNode n g of
                Nothing -> pure False 
                Just _ -> pure True

lastState :: [(ID, Char)] -> Graph -> GraphNode
lastState [] g = rootNode g
lastState xs Graph{..} = M.findWithDefault rootNode (fst (last xs)) nodes

walkPrefix :: String -> Graph -> ID -> [(ID, Char)]
walkPrefix [] _ _ = []
walkPrefix xs g _id = go xs g _id [] where
        go :: String -> Graph -> ID -> [(ID, Char)] -> [(ID, Char)]
        go [] _ _ acc = acc
        go (x : xs') Graph{..} _id acc = case M.lookup (_id, x) transitions of
                Nothing    -> acc
                Just newId -> go xs' g newId (acc ++ [(newId, x)])

mkNewNode :: Int -> GraphNode
mkNewNode _id = GraphNode {
             nodeId = coerce _id,
             nodeType = NonTerminating,
             nodeChildTransitions = S.empty 
        }

nodeHasChildren :: GraphNode -> Bool
nodeHasChildren = not . null . nodeChildTransitions

-- TODO: stop using fromJust/maximum
getLastChild :: GraphNode -> Graph -> GraphNode
getLastChild n g = 
        -- trace ("getLastChild of " <> show n) (
        let !lastChildTransition = S.findMax (nodeChildTransitions n)
            nextNodeId = fromJust (M.lookup (nodeId n, lastChildTransition) (transitions g))
        in fromJust (M.lookup nextNodeId (nodes g))

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = ifM c t (pure ())
{-# INLINE whenM #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t f = c >>= bool f t
{-# INLINE ifM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c f = ifM c (pure ()) f
{-# INLINE unlessM #-}