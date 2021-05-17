{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module DAFSA.GraphDot where

-- import qualified DAFSA.Internal.GraphBuilder as Builder
-- import DAFSA.Internal.GraphBuilder (GraphBuilder(..))
-- import Text.Dot ( attribute, edge, node, showDot, Dot (..), NodeId, userNode, userNodeId )
-- import Data.Coerce (coerce)
-- import qualified Data.Set as Set
-- import Data.Set (Set)
-- import qualified Data.IntSet as IS
-- import Data.IntSet ( IntSet )
-- import Data.Bool (bool)
-- import qualified Data.Map.Strict as M
-- import qualified Data.List as L
-- import Control.Monad.ST.Strict (runST)

-- toDotGraph :: GraphBuilder s -> Dot (Set (Int, Int, Char))
-- toDotGraph g@GraphBuilder{..} = do
--     attribute ("rankdir", "LR")
--     rootDotNode <- node [("label", show 0), shapeOf 0 fgFinalStates]
--     let children = childNodesOf 0 g
--         edges = Set.empty 
--     childrenToDotGraph g 0 rootDotNode edges children

-- childNodesOf :: Int -> GraphBuilder s -> [(Char, Int)]
-- childNodesOf parentId g = (\((_, chr), cId) -> (chr, cId)) <$> filter (\((_id, _), _) -> parentId == _id) (M.toList (fgTransitions g))


-- childrenToDotGraph :: GraphBuilder s -> Int -> NodeId -> Set (Int, Int, Char) -> [(Char, Int)] -> Dot (Set (Int, Int, Char)) 
-- childrenToDotGraph _ _ _ edges [] = return edges
-- childrenToDotGraph g parentId parentDotNode edges ((transition, childId) : rest) = do
--     userNode (userNodeId childId) [("label", show childId), shapeOf childId (fgFinalStates g)]
--     let edgeAdded = pure (Set.member (parentId, childId, transition) edges)
--     unlessM edgeAdded (edge parentDotNode (userNodeId childId) [("label", show transition)])
--     let newEdges = Set.insert (parentId, childId, transition) edges
--     childEdges <- childrenToDotGraph g childId (userNodeId childId) newEdges (childNodesOf childId g)
--     childrenToDotGraph g parentId parentDotNode childEdges rest

-- shapeOf :: Int -> IntSet -> (String, String)
-- shapeOf _id finalStates = bool ("shape", "circle") ("shape", "doublecircle") (IS.member _id finalStates)

-- runExampleGraph :: IO ()
-- runExampleGraph = do
--     let g = runST (Builder.build (L.sort ["mon", "thurs", "tues", "zon"]))
--     print g 
--     -- let !g = fromWords [ "\NUL\1034845" , "\SOH\1034845\NUL" ]
--     -- let !g = fromWords ["nikola", "bakola", "dukola", "zontan", "siklama", "sikl"]
--     -- let !dottedShow = showDot (toDotGraph g)
--     -- writeFile "graph" dottedShow
--     pure undefined