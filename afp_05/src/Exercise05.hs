{-# LANGUAGE ImportQualifiedPost #-}

module Exercise05 (main) where

import Control.Monad.ST (ST)
import Data.Array qualified as A
import Data.Array.MArray qualified as M
import Data.Array.ST
import Data.Array.Unboxed qualified as U
import Data.List (sort)
import State
import System.TimeIt
import Test.LeanCheck

{-
    Task 1 (3 points): Labeling terms
-}
data Term f v = Var v | Fun f [Term f v] deriving (Eq, Ord, Show)

type LTerm f v = Term (Int, f) (Int, v)

testTerm = Fun "f" [Var "x", Fun "g" [Var "y"], Var "x"]

labelTerm :: Term f v -> LTerm f v
labelTerm t = evalState (labelM t) 0
  where
    fresh :: State Int Int
    fresh = do
      n <- get
      put (n + 1)
      return n

    labelM :: Term f v -> State Int (LTerm f v)
    labelM (Var v) = do
      i <- fresh
      return (Var (i, v))
    labelM (Fun f ts) = do
      i <- fresh
      ts' <- mapM labelM ts
      return (Fun (i, f) ts')

{-
    Task 2 (7 points): minsort
-}

-- Functional Style

minSort :: (Ord a) => [a] -> [a]
minSort [] = []
minSort xs = m : minSort rest
  where
    (m, rest) = selectMin xs

    selectMin [y] = (y, [])
    selectMin (y : ys)
      | y <= m' = (y, ys)
      | otherwise = (m', y : rest')
      where
        (m', rest') = selectMin ys

-- Monadic Style
minSortM :: (MArray a e m, Ord e) => [e] -> m (a Int e)
minSortM xs = do
  arr <- M.newListArray (1, n) xs
  outer arr 1
  return arr
  where
    n = length xs

    outer arr i
      | i >= n = return ()
      | otherwise = do
          minIdx <- findMin arr i i
          swap arr i minIdx
          outer arr (i + 1)

    findMin arr minI j
      | j > n = return minI
      | otherwise = do
          vMin <- M.readArray arr minI
          vJ <- M.readArray arr j
          if vJ < vMin
            then findMin arr j (j + 1)
            else findMin arr minI (j + 1)

    swap arr i j
      | i == j = return ()
      | otherwise = do
          vi <- M.readArray arr i
          vj <- M.readArray arr j
          M.writeArray arr i vj
          M.writeArray arr j vi

-- wrapper to generic monadic implementation

minSortST :: (Ord a) => [a] -> [a]
minSortST xs = A.elems (runSTArray (minSortM xs))

minSortSTU :: (Ord a, U.IArray U.UArray a, forall s. MArray (STUArray s) a (ST s)) => [a] -> [a]
minSortSTU xs = U.elems (runSTUArray (minSortM xs))

-- tests

testTime f xs = do
  timeIt (putStrLn (show (sum (f xs))))

testSortAlg f = checkFor 10000 $ \xs -> sort xs == f (xs :: [Int])

main = do
  let xs = [1 .. 20000 :: Int]
  let ys = xs ++ reverse xs
  putStrLn (show (sum ys))
  putStrLn "minSort functional"
  testSortAlg minSort
  testTime minSort ys
  putStrLn "minSort ST"
  testSortAlg minSortST
  testTime minSortST ys
  putStrLn "minSort STU"
  testSortAlg minSortSTU
  testTime minSortSTU ys