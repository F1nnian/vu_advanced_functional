module Demo04_Functor where

-- Functors and Functor-instances are predefined;
-- we hide the existing definitions here to show how it can be done manually

import Prelude hiding (Functor, fmap, map)
import Data.Char(toUpper)

sqrtInt :: Int -> Double
sqrtInt x = sqrt (fromIntegral x)

sqrtList [] = []
sqrtList (x : xs) = sqrtInt x : sqrtList xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

-- same result
testList :: [Int] 
testList = [1,9,2] 
test1 = sqrtList testList
test2 = map sqrtInt testList

upperString = map toUpper


data Tree a = 
   Leaf a
 | Node (Tree a) (Tree a) 
   deriving Show
   
sqrtTree (Leaf x) = Leaf (sqrtInt x)
sqrtTree (Node l r) = Node (sqrtTree l) (sqrtTree r)   

testTree :: Tree Int
testTree = Node (Leaf 1) (Node (Leaf 9) (Leaf 2))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- same result
test3 = sqrtTree testTree
test4 = mapTree sqrtInt testTree

upperStringTree = mapTree upperString


class Functor f where
  fmap :: (a -> b) -> f a -> f b
  
instance Functor Tree where
  fmap = mapTree
  
instance Functor [] where
  fmap = map
  
instance Functor Maybe where
  fmap g Nothing = Nothing
  fmap g (Just x) = Just (g x)

fmapSqrt :: Functor f => f Int -> f Double
fmapSqrt = fmap sqrtInt
  
test5 = fmapSqrt testTree
test6 = fmapSqrt testList
test7 = fmapSqrt (Just 4)


safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

unsafeSquareAfterDiv x y = (^2) $ x `div` y
safeSquareAfterDiv x y = (^2) <$> x `safeDiv` y

instance Functor (Either a) where
  fmap f (Left x) = Left x
  fmap f (Right y) = Right (f y)
  
instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)

instance Functor ((,,) a b) where
  fmap f (x,y,z) = (x, y, f z)


test8 = fmap sqrtInt (1, 2)

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  first :: (a -> b) -> p a c -> p b c
  second :: (b -> c) -> p a b -> p a c
  first f = bimap f id 
  second f = bimap id f

instance Bifunctor Either where
  bimap f g (Left x) = Left (f x)
  bimap f g (Right y) = Right (g y)
  
instance Bifunctor (,) where
  bimap f g (x,y) = (f x, g y)

instance Bifunctor ((,,) a) where
  bimap f g (x,y,z) = (x, f y, g z)



