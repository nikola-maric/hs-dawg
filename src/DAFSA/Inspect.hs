{-# LANGUAGE BangPatterns #-}
module DAFSA.Inspect where

import DAFSA.Node (nodeFromList, NodeType(..), Node(..))
import Control.Monad.State.Strict
    ( MonadIO(liftIO),
      StateT(runStateT),
      MonadState(put, get),
      evalStateT )
import qualified Data.HashMap.Strict as Map
import Text.Dot ( attribute, edge, node, showDot, Dot, NodeId )

-- Node representation used for display purposes
data DotNode = DotNode {
    dnNode :: Node,
    dnId :: Int,
    dnType :: NodeType,
    dnChildren :: [(String, DotNode)]
}

toDot :: DotNode -> Dot NodeId
toDot _node = do
    attribute ("rankdir", "LR")
    curr <- node [("label", show (dnId _node)), shapeOf _node]
    childrenToDot curr (dnChildren _node)

childrenToDot :: NodeId -> [(String, DotNode)] -> Dot NodeId
childrenToDot nId [] = return nId
childrenToDot parent ((s, b) : xs) = do
    curr <- node [("label", show (dnId b)), shapeOf b]
    edge parent curr [("label", s)]
    _ <- childrenToDot curr (dnChildren b)
    childrenToDot parent xs

shapeOf :: DotNode -> (String, String)
shapeOf DotNode { dnType = Terminating }    = ("shape", "doublecircle")
shapeOf DotNode { dnType = NonTerminating } = ("shape", "circle")

labelExec :: Node -> IO DotNode
labelExec n = evalStateT (label n) [1..]

label :: Node -> StateT [Int] IO DotNode
label original_node@(Node node_type _map) = do
    (current_label : future_labels) <- get
    put future_labels
    others <- traverse lebelWithEdge (Map.toList _map)
    return DotNode { dnNode = original_node, dnType = node_type, dnId = current_label, dnChildren = others }     

lebelWithEdge :: (Char, Node) -> StateT [Int] IO (String, DotNode)
lebelWithEdge (transition, _node) = do
    labels <- get
    (labeled_node, future_labels) <- liftIO $ runStateT (label _node) labels
    put future_labels
    return (show transition, labeled_node)

runExample :: IO ()
runExample = do
    let g = nodeFromList ["nikola", "nik", "blakonik", "zomg", "tom", "tomg"]
    labeled <- labelExec g
    let !dottedShow = showDot (toDot labeled)
    writeFile "graph" dottedShow