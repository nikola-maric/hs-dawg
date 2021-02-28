{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module DAFSA.GraphDot where

import DAFSA.Graph
import Text.Dot ( attribute, edge, node, showDot, Dot, NodeId, userNode, userNodeId )
import Data.Coerce (coerce)
import qualified Data.Set as Set
import Data.Set (Set)

toDotGraph :: Graph -> Dot (Set (ID, ID, Char))
toDotGraph g@Graph{..} = do
    attribute ("rankdir", "LR")
    rootDotNode <- node [("label", show (nodeId rootNode)), shapeOf rootNode]
    let children = childGraphNodes rootNode g
        edges = Set.empty 
    childrenToDotGraph g rootNode rootDotNode edges children


childrenToDotGraph :: Graph -> GraphNode -> NodeId -> Set (ID, ID, Char) -> [(Char, GraphNode)] -> Dot (Set (ID, ID, Char)) 
childrenToDotGraph _ _ _ edges [] = return edges
childrenToDotGraph g parentNode parentDotNode edges ((transition, childNode) : xs) = do
    userNode (userNodeId (coerce (nodeId childNode))) [("label", show (nodeId childNode)), shapeOf childNode]
    let edgeAdded = pure (Set.member (nodeId parentNode, nodeId childNode, transition) edges)
    unlessM edgeAdded (edge parentDotNode (userNodeId (coerce (nodeId childNode))) [("label", show transition)])
    let newEdges = Set.insert (nodeId parentNode, nodeId childNode, transition) edges
    childEdges <- childrenToDotGraph g childNode (userNodeId (coerce (nodeId childNode))) newEdges (childGraphNodes childNode g)
    childrenToDotGraph g parentNode parentDotNode childEdges xs

shapeOf :: GraphNode  -> (String, String)
shapeOf GraphNode { nodeType = Terminating }    = ("shape", "doublecircle")
shapeOf GraphNode { nodeType = NonTerminating } = ("shape", "circle")

runExampleGraph :: IO ()
runExampleGraph = do
    let !g = fromWords ["mon", "thurs", "tues", "zon"]
    -- let !g = fromWords [ "\NUL\1034845" , "\SOH\1034845\NUL" ]
    let !dottedShow = showDot (toDotGraph g)
    writeFile "graph" dottedShow