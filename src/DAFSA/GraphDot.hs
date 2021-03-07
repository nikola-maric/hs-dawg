{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module DAFSA.GraphDot where

import DAFSA.Graph
import Text.Dot ( attribute, edge, node, showDot, Dot, NodeId, userNode, userNodeId )
import Data.Coerce (coerce)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntSet as IS
import Data.IntSet ( IntSet )
import Data.Bool (bool)
import qualified Data.Map.Strict as M

toDotGraph :: FinalGraph -> Dot (Set (Int, Int, Char))
toDotGraph g@FinalGraph{..} = do
    attribute ("rankdir", "LR")
    rootDotNode <- node [("label", show 0), shapeOf 0 fgFinalStates]
    let children = childNodesOf 0 g
        edges = Set.empty 
    childrenToDotGraph g 0 rootDotNode edges children

childNodesOf :: Int -> FinalGraph -> [(Char, Int)]
childNodesOf parentId g = (\((_, chr), cId) -> (chr, cId)) <$> filter (\((_id, _), _) -> parentId == _id) (M.toList (fgTransitions g))


childrenToDotGraph :: FinalGraph -> Int -> NodeId -> Set (Int, Int, Char) -> [(Char, Int)] -> Dot (Set (Int, Int, Char)) 
childrenToDotGraph _ _ _ edges [] = return edges
childrenToDotGraph g parentId parentDotNode edges ((transition, childId) : rest) = do
    userNode (userNodeId childId) [("label", show childId), shapeOf childId (fgFinalStates g)]
    let edgeAdded = pure (Set.member (parentId, childId, transition) edges)
    unlessM edgeAdded (edge parentDotNode (userNodeId childId) [("label", show transition)])
    let newEdges = Set.insert (parentId, childId, transition) edges
    childEdges <- childrenToDotGraph g childId (userNodeId childId) newEdges (childNodesOf childId g)
    childrenToDotGraph g parentId parentDotNode childEdges rest

shapeOf :: Int -> IntSet -> (String, String)
shapeOf _id finalStates = bool ("shape", "circle") ("shape", "doublecircle") (IS.member _id finalStates)

runExampleGraph :: IO ()
runExampleGraph = do
    let !g = fromWords ["mon", "thurs", "tues", "zon"]
    -- let !g = fromWords [ "\NUL\1034845" , "\SOH\1034845\NUL" ]
    let !dottedShow = showDot (toDotGraph g)
    writeFile "graph" dottedShow