{-
Data structure representing the pathfinding grid. 
-}

module Grid where

import Data.Hashable
import Data.HashMap.Strict

-- Square represents a single point on the grid. A Square may be one of four things:
-- - Open: An unvisited open space that can be entered with cost 1.
-- - Visited: A visited open space that should not be explored again
-- - Blocked: An closed space that cannot be entered.
-- - Start: The starting point. There should only be one per grid!
-- - Finish: The target endpoint. There should only be one per grid!
data Square = Open | Visited | Blocked | Start | Finish

instance Eq Square where
  Open    == Open    = True
  Visited == Visited = True
  Blocked == Blocked = True
  Start   == Start   = True
  Start   == Visited = True
  Finish  == Finish  = True
  _       == _       = False

instance Show Square where
  show Open    = "Open"
  show Visited = "Visited"
  show Blocked = "Blocked"
  show Start   = "Start"
  show Finish  = "Finish"


-- A grid coordinate
data Coord = Coord
  {
    x :: Int,
    y :: Int
  }

instance Eq Coord where
  Coord x1 y1 == Coord x2 y2 =
    if x1 == x2 && y1 == y2 then True else False

instance Show Coord where
  show (Coord x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Hashable Coord where
  hashWithSalt s (Coord x y) = s + (hash x) + (hash y)
  hash (Coord x y) = (hash x) + (hash y)
  
-- Maximum x-y dimensions of a grid. The minimum dimensions are always assumed to be 0.
data GridDims = GridDims
  {
    xmax :: Int,
    ymax :: Int
  }

-- A SquareMap maps grid Coordinates to Squares -- it tracks the status
-- of each grid Square
type SquareMap = HashMap Coord Square
emptySquareMap :: SquareMap
emptySquareMap = fromList ([] :: [(Coord, Square)])


-- Grid represents a 2D grid of GridPoints as a set
data Grid = Grid
  {
    dims :: GridDims,
    squares :: SquareMap
  }
  
instance Show Grid where
  show (Grid (GridDims xmax ymax) _) = "(" ++ show xmax ++ " x " ++ show ymax ++ ") Grid"

emptyGrid :: GridDims -> Grid
emptyGrid dims = (Grid dims emptySquareMap)

