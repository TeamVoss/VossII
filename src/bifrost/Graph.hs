module Graph where

import Data.List (nubBy)
import qualified Data.Map as M
import qualified Data.Set as S

type Graph a = M.Map a (S.Set a)

-- Find at least one cycle of a graph if any cycles exist.
findSomeCycles :: (Eq a, Ord a) => Graph a -> [[a]]
findSomeCycles graph = nubBy equiv $ M.keys graph >>= explore []
  where
    explore visited node =
      if node `elem` visited
        then [reverse visited]
        else
          let visited' = node:visited
              (Just neighborSet) = M.lookup node graph
              neighbors = S.toList neighborSet
          in neighbors >>= (explore visited')
    equiv c1 c2 = S.fromList c1 == S.fromList c2

-- Nodes reachable from a given set.
reachable :: (Eq a, Ord a) => Graph a -> S.Set a -> S.Set a
reachable g = foo
  where
    foo roots =
      let ns = S.unions $ map neighbors $ S.elems roots
          roots' = roots `S.union` ns
      in if roots == roots'
        then roots
        else foo roots'
    neighbors n =
      let (Just ns) = M.lookup n g
      in ns

-- Add an edge to a graph.
addEdge :: (Eq a, Ord a) => (a,a) -> Graph a -> Graph a
addEdge (from, to) g =
  case M.lookup from g of
    Just tos -> M.insert from (S.insert to tos) g
    Nothing -> M.insert from (S.singleton to) g
