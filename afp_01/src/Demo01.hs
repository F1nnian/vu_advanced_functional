module Demo01 where

import Data.List(foldl')

import qualified Data.Array.IArray as L  -- lazy, boxed arrays
import qualified Data.Array.Unboxed as S -- strict unboxed arrays
import qualified Data.Map.Lazy as M -- lazy maps
import Data.Word(Word64)

testL, testR, testL' :: Integer -> Integer
testL n = foldl (+) 0 [0..n]
testR n = foldr (+) 0 [0..n]
testL' n = foldl' (+) 0 [0..n]

testValue :: Integer
testValue = 10^(8 :: Int) 

findMinRepl :: Ord b => a -> [b] -> (b, [a])
findMinRepl r [x] = (x, [r])
findMinRepl r (x : xs) = case findMinRepl r xs of
  (m, ys) -> (min m x, r : ys)
  
replAllByMin :: Ord a => [a] -> [a] 
replAllByMin xs = 
  let (m, ys) = findMinRepl m xs
  in ys
  
fibsLazyArray :: Int -> [Integer]
fibsLazyArray n = 
  let a :: L.Array Int Integer
      a = L.genArray (0,n) 
         (\ i -> if i <= 1 then 1 else a L.! (i - 1) + a L.! (i - 2))
  in L.elems a

fibsStrictArray :: Int -> [Word64]
fibsStrictArray n = 
  let a :: S.UArray Int Word64
      a = S.genArray (0,n) 
         (\ i -> if i <= 1 then 1 else a S.! (i - 1) + a S.! (i - 2))
  in S.elems a

optBracketCosts :: [Integer] -> Integer
optBracketCosts xs = 
  let n = length xs - 1
      a = L.listArray (0,n) xs :: L.Array Int Integer
      m = M.fromList [((i,j),cost i j) | i <- [0..n - 1], j <- [i..n-1]]
      cost i j
        | i == j = 0
        | otherwise = minimum [costSplit k | k <- [i .. j - 1]] where
           costSplit k = 
             let c1 = m M.! (i,k)
                 c2 = m M.! (k+1,j)
             in c1 + c2 + a L.! i * a L.! (k + 1) * a L.! (j + 1)
  in cost 0 (n-1)