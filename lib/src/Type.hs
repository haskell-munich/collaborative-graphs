

module Type where

import qualified Data.Map as Map
import Data.Map (Map)

newtype NodeId = NodeId { unNodeId :: Int }
               deriving (Eq, Show, Read)
                        
newtype EdgeId = EdgeId { unEdgeId :: Int }
               deriving (Eq, Show, Read)


data Edge = Edge {
  from :: NodeId
  , to :: NodeId
  } deriving (Eq, Show, Read)


data NodeData = NodeData String
                
data Position = Position {
  xpos :: Int
  , ypos :: Int
  } deriving (Eq, Show, Read)

data Node = Node {
  position :: Position
  } deriving (Eq, Show, Read)


data Graph = Graph {
  nodes :: Map NodeId Node
  , edges :: Map EdgeId Edge
  } deriving (Eq, Show, Read)


empty :: Graph
empty = Graph Map.empty Map.empty

addNode :: NodeId -> Node -> Graph -> Graph
addNode nodeId node g = g { nodes = Map.insert nodeId node (nodes g) }

addEdge :: EdgeId -> Edge -> Graph -> Graph
addEdge edgeId edge g = g { edges = Map.insert edgeId edge (edges g) }

diff :: Graph -> Graph -> Graph
diff g h = Graph {
  nodes = nodes g Map.\\ nodes h
  , eges = edges g map.\\ edges g
  }

patch :: Graph -> Graph -> Graph
patch g diff = Graph {
  nodes = nodes g `Map.union` nodes diff
  , edges = edges g `Map.union` edges diff
  }
