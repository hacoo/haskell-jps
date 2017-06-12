{-
Data structure representing the pathfinding grid. 
-}

module Grid where

import qualified Data.Map.Strict as Map
import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!))
-- import qualified Data.PQueue.Min as PQ
import qualified Data.IntPSQ as PSQ


{-
Damn! Any way to use these with Unboxed vectors?

-- Square represents a single point on the grid. A Square may be one of four things:
-- - Open: An unvisited open space that can be entered with cost 1.
-- - Visited: A visited open space that should not be explored again
-- - onPath: A node on the shortest path from source to target
-- - Blocked: An closed space that cannot be entered.
-- - Start: The starting point. There should only be one per grid!
-- - Finish: The target endpoint. There should only be one per grid!
data Square = Open | Visited | Blocked | Start | Finish

instance Eq Square where
  Open    == Open    = True
  Blocked == Blocked = True
  Start   == Start   = True
  Finish  == Finish  = True
  _       == _       = False

instance Show Square where
  show Open    = "Open"
  show Blocked = "Blocked"
  show Start   = "Start"
  show Finish  = "Finish"
-}


-- Squares are represented by simple Word8's so they can be used in
-- Unboxed vectors. This sucks, I want to be able to associate them with their
-- labels! Anyway, here's waht each value means:
-- 0 : Blocked
-- 1 : Open
-- 2 : Start
-- 3 : Finish
-- 4 : Visited
-- 5 : onPath

type Square = Word8

-- Could refactor Int / Coordinate indices into a typeclass later if desired
-- a grid coordinate as a 2d (x,y) pair
data Coord = Coord 
  {
    x :: Int,
    y :: Int
  }

data Direction = N | NE | E | SE | S | SW | W | NW | C deriving (Eq, Show)
{-
instance Show Direction where
  show N  = "N"
  show NE = "NE"
  show E  = "E"
  show SE = "SE"
  show S  = "S"
  show SW = "SW"
  show W  = "W"
  show NW = "NW"
-}
-- Functions for moving in each direction
n :: GridDims -> Int -> Int
n d i = i - (xmax d)

s :: GridDims -> Int -> Int
s d i = i + (xmax d)

e :: GridDims -> Int -> Int
e d i = i + 1

w :: GridDims -> Int -> Int
w d i = i - 1

ne :: GridDims -> Int -> Int
ne d i = e d $ n d i

nw :: GridDims -> Int -> Int
nw d i = w d $ n d i

se :: GridDims -> Int -> Int
se d i = e d $ s d i 

sw :: GridDims -> Int -> Int
sw d i = w d $ s d i

moveInDirection :: GridDims -> Direction -> Int -> Int
moveInDirection d dir i = case dir of
                            N  -> (n d i)
                            NE -> (ne d i)
                            E  -> (e d i)
                            SE -> (se d i)
                            S  -> (s d i)
                            SW -> (sw d i)
                            W  -> (w d i)
                            NW -> (nw d i)
                            C  -> i

between :: Direction -> Direction -> Direction
between d1 d2 = case (d1, d2) of
                  (N, E) -> NE
                  (E, N) -> NE
                  (E, S) -> SE
                  (S, E) -> SE
                  (S, W) -> SW
                  (W, S) -> SW
                  (W, N) -> NW
                  (N, W) -> NW
                  _      -> error $ ("Tried to find direction between non-orthogonal directions: "
                                      ++ show d1 ++ " and " ++ show d2)
                            
nineties :: Direction -> (Direction, Direction)
nineties d = case d of
               N  -> (W, E)
               NE -> (NW, SE)
               E  -> (N, S)
               SE -> (NE, SW)
               S  -> (E, W)
               SW -> (SE, NW)
               W  -> (S, N)
               NW -> (SW, NE)
               C  -> error $ "nineties relative center is undefined"


fortyfives :: Direction -> (Direction, Direction)
fortyfives d = case d of
                 N  -> (NE, NW)
                 NE -> (N, E)
                 E  -> (NE, SE)
                 SE -> (E, S)
                 S  -> (SE, SW)
                 SW -> (S, W)
                 W  -> (SW, NW)
                 NW -> (W, N)
                 C  -> error $ "fortyfives relative center is undefined"

opposite :: Direction -> Direction
opposite d = case d of
               N  -> S
               NE -> SW
               E  -> W
               SE -> NW
               S  -> N
               SW -> NE
               W  -> E
               NW -> SE
               C  -> C
 

-- Calculates the infinity norm between two Coords
{-
linf :: Coord -> Coord -> Int
linf (Coord x1 y1) (Coord x2 y2) = let
  xdif = abs (x1-x2)
  ydif = abs (y1-y2)
  in
    if xdif >= ydif then xdif else ydif
{-# INLINE linf #-}
-}

chebyshev :: Coord -> Coord -> Int
chebyshev (Coord x1 y1) (Coord x2 y2) = let
  xdif = abs (x2-x1)
  ydif = abs (y2-y1)
  in
    max xdif ydif
    

isInBounds :: GridDims -> Int -> Bool
isInBounds (GridDims xdim ydim) i = i > 0 && i < (xdim * ydim)
{-# INLINE isInBounds #-}

isOpen :: Pathfinding -> Int -> Bool
isOpen pf i =
  let
  (Grid dims squares) = grid pf
  v                   = visited pf
  in
    isInBounds dims i &&
    (squares ! i) > 0 &&
    not (Map.member i v)
{-# INLINE isOpen #-}
                   
isBlocked :: Grid -> Int -> Bool
isBlocked (Grid dims squares) i = (not (isInBounds dims i)) || ((squares ! i) == 0)
{-# INLINE isBlocked #-}

isFinish :: Pathfinding -> Int -> Bool
isFinish (Pathfinding g v o s f) i = i == f
{-# INLINE isFinish #-}

-- Convert a 2-d coordinate to a flattened index
c2i :: GridDims -> Coord -> Int
c2i (GridDims xdim ydim) (Coord x y) = y * ydim + x
{-# INLINE c2i #-}

-- Convert a flat index to a 2d coordinate
i2c :: GridDims -> Int -> Coord
i2c (GridDims xdim ydim) i = (Coord (i `rem` xdim) (i `div` xdim))
{-# INLINE i2c #-}

instance Eq Coord where
  Coord x1 y1 == Coord x2 y2 =
    if x1 == x2 && y1 == y2 then True else False

instance Show Coord where
  show (Coord x y) = "(" ++ show x ++ "," ++ show y ++ ")"
  
-- Maximum x-y dimensions of a grid. The minimum dimensions are always assumed to be 0.
data GridDims = GridDims
  {
    xmax :: Int,
    ymax :: Int
  }

-- Grid -- represents a (stateless) search space
data Grid = Grid
  {
    dims    :: GridDims,
    squares :: U.Vector Square
  }
  
instance Show Grid where
  show (Grid (GridDims xmax ymax) _) = "(" ++ show xmax ++ " x " ++ show ymax ++ ") Grid"

-- represents a starting node of a search in progress
data SearchNode = SearchNode
  {
    prev     :: Maybe SearchNode,
    index    :: Int,
    dir      :: Direction,
    depth    :: Int
  }

instance Show SearchNode where
  show (SearchNode pr c d dep) = "(SearchNode: { " ++ (show pr) ++ "," ++ (show c) ++ "," ++ (show dep) ++ " })"

  
-- Represents pathfinding in progress
data Pathfinding = Pathfinding
  {
    grid    :: Grid,
    visited :: Map.Map Int Int,
    open    :: PSQ.IntPSQ Int SearchNode,
    start   :: Int,
    finish  :: Int
  }

newPathfinding :: Grid -> Int -> Int -> Pathfinding
newPathfinding grid start finish = (Pathfinding grid Map.empty PSQ.empty start finish)

emptyGrid :: GridDims -> Grid
emptyGrid dims = (Grid dims (U.fromList []))

-- Finds all valid open square indices around an index
getOpenAround :: Grid -> Int -> [(Direction, Int)]
getOpenAround g i = let
  d = dims g
  in 
    filter (\(dir, i) -> (not $ isBlocked g i))
    [(NW, nw d i), (N, n d i), (NE, ne d i),
     (W,  w  d i),             (E,  e  d i),
     (SW, sw d i), (S, s d i), (SE, se d i)]

-- Turns a path beginning at finish into a list of visited nodes
makePath :: Map.Map Int Int -> Int -> Int -> [Int]
makePath visited current start =
  if start == current then
    [start]
  else
    let previous = Map.lookup current visited in
      case previous of
        Just p  -> current : (makePath visited p start)
        Nothing -> error "Error -- node wasn't in the visited map when backtracing path"
      
-- Mark all Visited and onPath nodes in a grid based on search result
markGrid :: Pathfinding -> [Int] -> Grid
markGrid pf path =
  let
    ds  = dims $ grid pf
    sqs = squares $ grid pf
    v   = visited pf
    pathmap = Map.fromList (map (\ x -> (x, 1)) path)
    visitedMarked = U.imap (markSquare v 4) sqs
    pathMarked = U.imap (markSquare pathmap 5) visitedMarked
  in
    (Grid ds pathMarked)
                                
-- Marks squares with the marker value. If any of the squares are Blocked, will throw
-- and error as a sanity check
markSquares :: Map.Map Int Int -> U.Vector Square -> Square -> U.Vector Square
markSquares colormap sqs color = U.imap (markSquare colormap color) sqs

markSquare :: Map.Map Int Int -> Square -> Int -> Square -> Square
markSquare colormap color i sq = if Map.member i colormap then color else sq
                                    
