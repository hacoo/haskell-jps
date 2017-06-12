-- Implements A-star search framework and basic heuristic

module Astar where

import qualified Data.Map.Strict as Map
import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!))
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.IntPSQ as PSQ

import Grid

import Debug.Trace


-- an a-star expansion method. Examines the current pathfinding state and finds new nodes; returns
-- a list of the new searchnodes
--type ExpandFn = Pathfinding -> Int -> Int -> PSQ.IntPSQ Int SearchNode
type ExpandFn = Pathfinding -> SearchNode -> [SearchNode]
type HeuristicFn = SearchNode -> Pathfinding -> Int

-- An a-star heuristic. Examines the current pathfinding state and a square index, and returns a value.
-- Lower is better.

-- pathfind from Start to Finish. Return the path and a grid, with each node marked according to whether it was
-- visited or on-path. Uses normal a-star heuristic.
findPathNormal :: Grid -> Int -> Int -> (Maybe [Int], Grid)
findPathNormal g start finish =
  let
    pf          = newPathfinding g start finish
    startnode   = (SearchNode start start C 0)
  in
    astar normalExpand normalHeuristic pf startnode

    
-- The normal A* heuristic. Uses infinity norm, since we can move in all 8 directions on our grid
normalHeuristic :: HeuristicFn
normalHeuristic node (Pathfinding grid visited open start finish) = let
  dim   = dims grid
  p     = current node
  dist  = chebyshev (i2c dim p) (i2c dim finish)
  in
    dist + (depth node)

-- Normal expansion method, just expands to all adjacent nodes
normalExpand :: ExpandFn
normalExpand pf (SearchNode prev curr d depth) =
  let
    squaresToAdd = getOpenAround pf curr
    depth'       = depth+1
  in
     [(SearchNode  curr i dir depth') | (dir, i) <- squaresToAdd]

-- Utility functions, dumps a list of SearchNodes to a PQ
addToPQ :: PSQ.IntPSQ Int SearchNode -> [(Int, SearchNode)] -> PSQ.IntPSQ Int SearchNode
addToPQ open nodes = L.foldl' (\ pq (priority, node) -> PSQ.insert (current node) priority node pq) open nodes

-- Runs a-star search with the specific expansion function and heuristic function.
-- Returns the optimal path as a list of integers, and a Grid, updated to show the path
-- and visited nodes.
astar :: ExpandFn    ->
         HeuristicFn ->
         Pathfinding ->
         SearchNode  ->
         (Maybe [Int], Grid)
astar expand heuristic pf sn =
  let
    (Pathfinding grid visited open start finish) = pf
    (SearchNode prev curr dir depth) = sn
    visited' = Map.insert curr prev visited
    pf' = (Pathfinding grid visited' open start finish)
    newnodes = expand pf' sn
    nodeswithcosts = [(heuristic n pf, n) | n <- newnodes]
    open'    = addToPQ open nodeswithcosts
  in
    if curr == finish then
      let path = makePath visited' curr start in
        (Just path, markGrid pf path)
    else
      case PSQ.findMin open' of
        Just (k, p, searchnode) -> let pf' = (Pathfinding grid visited' (PSQ.deleteMin open') start finish) in
                                         astar expand heuristic pf' searchnode
        Nothing                 -> (Nothing, markGrid (Pathfinding grid visited' open' start finish) [])

  
