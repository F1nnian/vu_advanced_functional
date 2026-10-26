{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Exercise03 where


-- Task 1

-- fun :: TODO
-- fun c = c 'i' c


-- an abstract map interface
class Map m a where
  -- a = key type, b = value type
  empty :: m a b
  get :: m a b -> a -> Maybe b     -- lookup via key
  put :: m a b -> a -> b -> m a b  -- insert (or overwrite) entry for key
  toList :: m a b -> [(a,b)]       -- convert map to list of key-value pairs

-- AList = association lists to implement maps 
-- ALists are lists of (key,value) pairs, where each key occurs at most once
newtype AList a b = AList [(a,b)]

-- Task 2.2

instance Eq a => Map AList a where
  empty = AList []
  get = undefined
  put = undefined
  toList = undefined

-- binary (unbalanced) trees to implement maps
data Tree a b = Empty | Node (Tree a b) a b (Tree a b)

instance Ord a => Map Tree a where
  empty = Empty
  get = undefined
  put = undefined
  toList = undefined


-- Task 2.3

{-
  Below you find an algorithm to compute the set of all reachable
  nodes from a starting node in a directed graph. Here, in the output
  one sees full paths ([a]), however the paths are not necessarily shortest
  paths. 
  In order to store the set of visited nodes, the implementation uses
  a map from reachable nodes to paths, hard-coded as a list.

-}
-- directed graphs as list of edges (source, target)
type Graph a = [(a,a)]

-- a test type for nodes
data GraphNode = A | B | C | D | E 
  deriving (Eq, Show, Ord)

testGraph :: Graph GraphNode
testGraph = [(A, B), (A, A), (B, D), (C, D), (D, B), (E, C), (E, A)]

reachMainList :: Eq a => Graph a -> [(a,[a])] -> [(a,[a])] -> [(a,[a])]
reachMainList g paths [] = paths
reachMainList g paths ((x,p) : todos) = case lookup x paths of 
  Just _ -> reachMainList g paths todos
  Nothing -> let 
    newPaths = (x,p) : paths
    xSuccessors = [(y, y:p) | (src,y) <- g, src == x]
   in reachMainList g newPaths (xSuccessors ++ todos)

reach :: Eq a => Graph a -> a -> [[a]]
reach g start = map (reverse . snd) $ reachMainList g [] [(start, [start])]

testA = reach testGraph A
testE = reach testGraph E


-- TODO: implement reachMain

-- reachMain :: TODO
reachMain g = undefined

-- Task 2.4

reachTree :: Eq a => Graph a -> a -> [[a]]
reachTree = undefined

reachAList :: Eq a => Graph a -> a -> [[a]]
reachAList = undefined
