module Exercise05 (main) where

import Control.Monad.ST (ST)
import qualified Data.Array as A
import qualified Data.Array.MArray as M
import Data.Array.ST
import qualified Data.Array.Unboxed as U
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
minSort = undefined

-- Monadic Style
minSortM :: (MArray a e m, Ord e) => [e] -> m (a Int e)
minSortM = undefined

-- wrapper to generic monadic implementation

minSortST :: (Ord a) => [a] -> [a]
minSortST = undefined

minSortSTU :: [Int] -> [Int]
minSortSTU = undefined

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
