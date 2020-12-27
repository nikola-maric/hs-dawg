{-# LANGUAGE DeriveGeneric #-}
module DAFSA.Graph where

import DAFSA.Node ( Node(..), nodeContains, addToNode, emptyNode )
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
-- import qualified Data.Map as Map
import qualified Data.HashMap.Strict as Map
import Control.Monad (join, forM_, forM)
import qualified Data.HashMap.Strict as HashMap

-- FST represented by root node, pointing to others
newtype Graph = Graph Node deriving (Eq, Show, Generic)

instance NFData Graph



graphContains :: String -> Graph -> Bool 
graphContains word _fst = nodeContains word (coerce _fst)

addToGraph :: String -> Graph -> Graph
addToGraph word _fst = coerce (addToNode word (coerce _fst))

graphFromList :: [String] -> Graph
graphFromList = foldr addToGraph emptyGraph

emptyGraph :: Graph
emptyGraph = Graph emptyNode