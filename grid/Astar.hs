-- Implements boring, bone-stock a-star search

module Astar where

import qualified Data.Map.Strict as Map
import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!))
import qualified Data.Set as Set
-- import qualified Data.PQueue.Min as PQ
import qualified Data.IntPSQ as PSQ
import qualified Data.List as L

import Grid

import Debug.Trace

-- pathfind from Start to Finish. Return a Grid with the path and visited nodes drawn on.
findPath :: Grid -> Int -> Int -> Maybe [Int]
findPath grid start finish =
  let
    pf        = newPathfinding grid start finish
    startnode = (SearchNode start start 0)
    path      = astar pf startnode
  in
    path
    
-- The normal A* heuristic. Uses infinity norm, since we can move in all 8 directions on our grid
heuristic :: Int ->  Pathfinding -> Int
heuristic p (Pathfinding grid visited open start finish) =
  let
    dim   = dims grid
  in
    linf (i2c dim p) (i2c dim finish)
     
expand :: Pathfinding -> Int -> Int -> PSQ.IntPSQ Int SearchNode
expand pf curr depth =
  let
    squaresToAdd = getOpenAround pf curr
    depth'       = depth+1
    searchnodes  = [((heuristic i pf) + depth', (SearchNode  curr i depth')) | i <- squaresToAdd]
  in
    -- An example of trace!!!
    --trace (show ((squares (grid pf)) ! curr)) $ L.foldl' (\ pq node -> PQ.insert node pq) (open pf) searchnodes
    L.foldl' (\ pq (priority, node) -> PSQ.insert (current node) priority node pq) (open pf) searchnodes

astar :: Pathfinding -> SearchNode -> Maybe [Int]
astar pf (SearchNode prev curr depth) =
  let
    (Pathfinding grid visited open start finish) = pf
    visited' = Map.insert curr prev visited
    open'    = expand pf curr depth
  in
    if curr == finish then
      trace "Done" $ Just (makePath visited' curr start)
    else
      case PSQ.findMin open' of
        Just (k, p, searchnode) -> astar (Pathfinding grid visited' (PSQ.deleteMin open') start finish) searchnode
        Nothing         -> Nothing
              
