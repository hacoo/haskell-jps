-- Implements Jump Point Search

module JPS where

import qualified Data.Map.Strict as Map
import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!))
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.IntPSQ as PSQ

import Grid
import Astar

import Debug.Trace

data JumpPoint = JumpPoint
  {
    d     :: Direction,
    i     :: Int
  }

data Jump = Jump
  {
    dir   :: Direction,
    start :: Int,
    end   :: Int
  }

findPathJPS :: Grid -> Int -> Int -> (Maybe [Int], Grid)
findPathJPS g start finish =
  let
    pf          = newPathfinding g start finish
    startnode   = (SearchNode start start C 0)
  in
    astar expandJPS heuristicJPS pf startnode

-- Gets direction of dest relative to src
pairToDirection :: GridDims -> Int -> Int -> Direction 
pairToDirection d src dest 
  | dest == (n d src)  = N
  | dest == (ne d src) = NE
  | dest == (e d src)  = E
  | dest == (se d src) = SE
  | dest == (s d src)  = S
  | dest == (sw d src) = SW
  | dest == (w d src)  = W
  | dest == (nw d src) = NW
  | otherwise          = C


-- Figurs out which direction to scan in and proceed
expandJPS :: ExpandFn
expandJPS pf sn = let (SearchNode prev curr dir depth) = sn in
    case dir of 
      C -> normalExpand pf sn
      _ -> scan pf sn

heuristicJPS :: HeuristicFn
heuristicJPS = normalHeuristic
    
-- Rules for scanning in each direction are below. Scans will continue in the specified direction
-- until they are blocked, hit the finish, or encounder a forced neighbor point. When forced neighbors
-- are encountered, scanning will stop, and the new jump points are returned. 

-- I wonder if there's a better way to do this? I wasn't able to think of a reasonably concise way to express
-- the JPS rules, other than just enumerating them.

-- infinity norm between two search nodes
--slinf :: GridDims -> SearchNode -> SeachNode -> Int
--snlinf g (SearchNode _ c1 _ _) (SearchNode _ c2 _ _)

-- Creates a searchnode relative to i if the target square is not blocked; otherwise returns empty list
createSN :: Grid -> Int -> Int -> Direction -> Int -> [SearchNode]
createSN g st i dir depth =
  let d = dims g
      t = moveInDirection d dir i
  in
    if isBlocked g t then [ ] else [(SearchNode st t dir depth)]

-- Creates a searchnode at dir2 if dir1 is blocked, and dir2 is not
createSNIfBlocked :: Grid -> Int -> Int -> Direction -> Direction -> Int -> [SearchNode]
createSNIfBlocked g st i dir1 dir2 depth =
  let d  = dims g
      s1 = moveInDirection d dir1 i
      s2 = moveInDirection d dir2 i
  in
    if isBlocked g s1 && not (isBlocked g s2) then [(SearchNode st s2 dir2 depth)] else [ ]

scan :: Pathfinding -> SearchNode -> [SearchNode]
scan pf sn 
  | dir `elem` [N, E, S, W]  = scanStraight pf sn c depth
  | otherwise                = scanDiag pf sn c depth
    where (SearchNode p c dir depth) = sn

-- Diagonal scan does not need to check for forced neighbors -- the straight scans that it spawns
-- will catch them! COOL!!!
scanDiag :: Pathfinding -> SearchNode -> Int -> Int -> [SearchNode]
scanDiag pf sn i dep
  | (isBlocked g i)                   =  [ ]
  | (isFinish  pf i)                  =  [(SearchNode st i C 0)]
  | otherwise                         =  stScans ++ (scanDiag  pf sn (moveInDirection d dir i) (dep + 1))
  where
    g                          = grid pf
    d                          = dims g
    (SearchNode prev st dir _) = sn
    (f1, f2)                   = fortyfives dir
    stScans                    = (scanStraight pf (SearchNode prev st f1 dep) i dep) ++
                                 (scanStraight pf (SearchNode prev st f2 dep) i dep)

scanStraight :: Pathfinding -> SearchNode -> Int -> Int -> [SearchNode]
scanStraight pf sn i dep
  | (isBlocked g i)                   =  [ ]
  | (isFinish  pf i)                  =  [(SearchNode st i C 0)]
  | hasForcedNeighborsStraight g sn i = forcedNeighborsStraight g sn i dep
  | otherwise                         = scanStraight pf sn (moveInDirection d dir i) (dep + 1)
  where
    g = grid pf
    d = dims g
    (SearchNode prev st dir _) = sn

hasForcedNeighborsStraight :: Grid -> SearchNode -> Int -> Bool
hasForcedNeighborsStraight g (SearchNode p c dir dep) i = let
  (d1, d2) = nineties dir
  d        = dims g
  in
    isBlocked g (moveInDirection d d1 i) || isBlocked g (moveInDirection d d2 i)
  
forcedNeighborsStraight :: Grid -> SearchNode -> Int -> Int -> [SearchNode]
forcedNeighborsStraight g (SearchNode p c dir _) i dep = let
  (d1, d2) = nineties dir
  continue = createSN g c i dir (dep + 1)
  forced1  = createSNIfBlocked g c i d1 (between dir d1) (dep + 1)
  forced2  = createSNIfBlocked g c i d2 (between dir d2) (dep + 1)
  in
    continue ++ forced1 ++ forced2
    



{-
scanW :: Grid -> Int -> SearchNode -> [SearchNode]
scanW g st i
  | (isBlocked g i)                                = [ ]
  | (isFinish g i)                                 = [(SearchNode (e d i) i C 0)]
  | (isBlocked g (n d i)) || (isBlocked g (s d i)) = forcedNeighborsW g st i
  | otherwise                                      = scanE g st (w d i)  
  where d = dims g

forcedNeighborsW :: Grid -> Int -> Int ->  [SearchNode]
forcedNeighborsW g st i = let
  d      = dims g
  diagN  = if isBlocked g (n d i) then [(SearchNode st (nw d i) NW 0)] else [ ]
  diagS  = if isBlocked g (s d i) then [(SearchNode st (sw d i) SW 0)] else [ ]
  horizW = if isBlocked g (w d i) then [(SearchNode st  (w d i) W 0)] else [ ]
  in diagN ++ diagS ++ horizW

scanN :: Grid -> Int -> Int -> [SearchNode]
scanN g st i
  | (isBlocked g i)                                = [ ]
  | (isFinish g i)                                 = [(SearchNode (s d i) i C 0)]
  | (isBlocked g (w d i)) || (isBlocked g (e d i)) = forcedNeighborsN g st i
  | otherwise                                      = scanN g st (n d i)  
  where d = dims g

forcedNeighborsN :: Grid -> Int -> Int ->  [SearchNode]
forcedNeighborsN g st i = let
  d      = dims g
  diagE  = if isBlocked g (e d i) then [(SearchNode st (ne d i) NE 0)] else [ ]
  diagW  = if isBlocked g (w d i) then [(SearchNode st (nw d i) NW 0)] else [ ]
  vertS  = if isBlocked g (n d i) then [(SearchNode st  (n d i) N 0)]  else [ ]
  in diagE ++ diagW ++ vertS

scanS :: Grid -> Int -> Int -> [SearchNode]
scanS g st i
  | (isBlocked g i)                                = [ ]
  | (isFinish g i)                                 = [(SearchNode (n d i) i C 0)]
  | (isBlocked g (w d i)) || (isBlocked g (e d i)) = forcedNeighborsS g st i
  | otherwise                                      = scanS g st (s d i)  
  where d = dims g

forcedNeighborsS :: Grid -> Int -> Int ->  [SearchNode]
forcedNeighborsS g st i = let
  d      = dims g
  diagE  = if isBlocked g (e d i) then [(SearchNode st (se d i) SE 0)] else [ ]
  diagW  = if isBlocked g (w d i) then [(SearchNode st (sw d i) SW 0)] else [ ]
  vertS  = if isBlocked g (s d i) then [(SearchNode st  (s d i) S 0)] else [ ]
  in diagE ++ diagW ++ vertS
-}


--hasForcedNeighbor :: Pathfinding -> SearchNode -> Bool
--hasForcedNeighbor pf node = 
