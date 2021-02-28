{-# LANGUAGE BangPatterns #-}
module DAFSA.Inspect where

-- import DAFSA.Node ( Node(..), NodeType (Terminating, NonTerminating), Transition, fromList )
-- import DAFSA.Id (ID(..))
-- import GHC.Base (coerce)
-- import qualified Data.HashMap.Strict as Map
-- import Text.Dot ( attribute, edge, node, showDot, Dot, NodeId )

-- data DotNode = DotNode {
--     dnNode :: Node,
--     dnId :: Int,
--     dnType :: NodeType,
--     dnChildren :: [(String, DotNode)]
-- } deriving Show

-- toDotNode :: Node -> DotNode
-- toDotNode n@(Node _id _type _others) = DotNode {
--     dnNode = n,
--     dnId = coerce _id,
--     dnType = _type,
--     dnChildren = toDotChildren _others
-- }

-- toDotChildren :: Map.HashMap Transition Node -> [(String, DotNode)]
-- toDotChildren transitions = fmap (\(t, n) -> ([t], toDotNode n)) (Map.toList transitions)

-- toDot :: Node -> Dot NodeId
-- toDot _node = do
--     attribute ("rankdir", "LR")
--     let dNode = toDotNode _node 
--     curr <- node [("label", show (dnId dNode)), shapeOf dNode]
--     childrenToDot curr (dnChildren dNode)

-- childrenToDot :: NodeId -> [(String, DotNode)] -> Dot NodeId
-- childrenToDot nId [] = return nId
-- childrenToDot parent ((s, b) : xs) = do
--     curr <- node [("label", show (dnId b)), shapeOf b]
--     edge parent curr [("label", s)]
--     _ <- childrenToDot curr (dnChildren b)
--     childrenToDot parent xs

-- shapeOf :: DotNode -> (String, String)
-- shapeOf DotNode { dnType = Terminating }    = ("shape", "doublecircle")
-- shapeOf DotNode { dnType = NonTerminating } = ("shape", "circle")

-- runExample :: IO ()
-- runExample = do
--     let g = fromList ["", "nikola", "nik", "blakonik", "zomg", "tom", "tomg"]
--     let !dottedShow = showDot (toDot g)
--     writeFile "graph" dottedShow