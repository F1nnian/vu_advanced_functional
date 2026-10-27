--
-- allows to define instances for type constructors with parameters (6.8.8)
{-# LANGUAGE FlexibleInstances #-}
-- allows to define type classes with multiple parameters (6.8.1)
{-# LANGUAGE MultiParamTypeClasses #-}

module Exercise03 where

-- Task 1

fun :: (forall a. Char -> a -> b) -> b
fun c = c 'i' c

-- an abstract map interface
class Map m a where -- wouldnt work without MultiParamTypeClasses, because we have two type parameters
-- a = key type, b = value type
  empty :: m a b
  get :: m a b -> a -> Maybe b -- lookup via key
  put :: m a b -> a -> b -> m a b -- insert (or overwrite) entry for key
  toList :: m a b -> [(a, b)] -- convert map to list of key-value pairs

-- AList = association lists to implement maps
-- ALists are lists of (key,value) pairs, where each key occurs at most once
newtype AList a b = AList [(a, b)]

-- Task 2.2

instance (Eq a) => Map AList a where
  empty = AList []
  get (AList xs) k = lookup k xs
  put (AList xs) k v = AList ((k, v) : filter (\(i, _) -> i /= k) xs)
  toList (AList xs) = xs

-- binary (unbalanced) trees to implement maps
data Tree a b = Empty | Node (Tree a b) a b (Tree a b)

instance (Ord a) => Map Tree a where
  empty = Empty
  get Empty _ = Nothing
  get (Node left k v right) key
    | key < k = get left key
    | key > k = get right key
    | otherwise = Just v
  put Empty key val = Node Empty key val Empty
  put (Node left k v right) key val
    | key < k = Node (put left key val) k v right
    | key > k = Node left k v (put right key val)
    | otherwise = Node left key val right
  toList Empty = []
  toList (Node left k v right) = toList left ++ [(k, v)] ++ toList right

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
type Graph a = [(a, a)]

-- a test type for nodes
data GraphNode = A | B | C | D | E
  deriving (Eq, Show, Ord)

testGraph :: Graph GraphNode
testGraph = [(A, B), (A, A), (B, D), (C, D), (D, B), (E, C), (E, A)]

reachMainList :: (Eq a) => Graph a -> [(a, [a])] -> [(a, [a])] -> [(a, [a])]
reachMainList g paths [] = paths
reachMainList g paths ((x, p) : todos) = case lookup x paths of
  Just _ -> reachMainList g paths todos
  Nothing ->
    let newPaths = (x, p) : paths
        xSuccessors = [(y, y : p) | (src, y) <- g, src == x]
     in reachMainList g newPaths (xSuccessors ++ todos)

reach :: (Eq a) => Graph a -> a -> [[a]]
reach g start = map (reverse . snd) $ reachMainList g [] [(start, [start])]

testA = reach testGraph A

testE = reach testGraph E

reachMain :: (Map m a, Eq a) => Graph a -> m a [a] -> [(a, [a])] -> [(a, [a])]
reachMain _ visited [] = toList visited
reachMain g visited ((x, p) : todos) = case get visited x of
  Just _ -> reachMain g visited todos
  Nothing ->
    let newVisited = put visited x p
        xSuccessors = [(y, y : p) | (src, y) <- g, src == x]
     in reachMain g newVisited (xSuccessors ++ todos)

-- Task 2.4

reachTree :: forall a. (Ord a) => Graph a -> a -> [[a]]
reachTree g start = map (reverse . snd) $ reachMain g (empty :: Tree a [a]) [(start, [start])]

reachAList :: forall a. (Eq a) => Graph a -> a -> [[a]]
reachAList g start = map (reverse . snd) $ reachMain g (empty :: AList a [a]) [(start, [start])]

testA_Tree = reachTree testGraph A

testE_Tree = reachTree testGraph E

testA_AList = reachAList testGraph A

testE_AList = reachAList testGraph E