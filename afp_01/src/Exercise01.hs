module Exercise01 where

import qualified Test.LeanCheck as C -- for checking exercises
import qualified Data.Array.IArray as L  -- lazy, boxed arrays
import qualified Data.Map.Lazy as M -- lazy maps

-- Task 1

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
  
data Brackets = Leaf | Split Brackets Int Brackets deriving (Eq,Show)

-- compute costs of given input dimensions with bracket-information
costOfBrackets :: [Integer] -> Brackets -> Integer
costOfBrackets xs = 
  let n = length xs - 1
      a = L.listArray (0,n) xs :: L.Array Int Integer
  in costBmain a 0 (n-1) 
 where
   costBmain _ i j Leaf 
    | i == j = 0
    | otherwise = error "Leaf for non-single matrix"
   costBmain a i j (Split bleft k bright)
    | i <= k && k < j = costBmain a i k bleft + costBmain a (k+1) j bright 
        + a L.! i * a L.! (k+1) * a L.! (j+1)
    | otherwise = error "out of bounds"

-- implement a function to compute an optimal bracketing information;
-- your implementation should require at most polynomial time
optBrackets :: [Integer] -> Brackets
optBrackets xs = 
  let n = length xs - 1
      a = L.listArray (0,n) xs :: L.Array Int Integer
      m = M.fromList [((i,j),cost i j) | i <- [0..n - 1], j <- [i..n-1]]
      cost i j
        | i == j = (0, Leaf)
        | otherwise = foldl1 (\x y -> if fst x <= fst y then x else y) 
                             [costSplit k | k <- [i .. j - 1]] where
           costSplit k = 
             let (c1, b1) = m M.! (i,k)
                 (c2, b2) = m M.! (k+1,j)
             in (c1 + c2 + a L.! i * a L.! (k + 1) * a L.! (j + 1), Split b1 k b2)
  in snd (m M.! (0, n - 1))
  
-- generate lists of length at least two with only positive numbers
processBracketInputs :: [Integer] -> [Integer]
processBracketInputs xs = map (\ x -> max (abs x) 1) 
  (if length xs <= 1 then [1,1] else xs)

-- invoke "testsBrackets" to test for optimality
-- check that computed optimal costs corresponds to result of brackets function
testsBrackets :: IO ()
testsBrackets = C.checkFor 1000 (\ xs -> 
  let ys = processBracketInputs xs
  in costOfBrackets ys (optBrackets ys) == optBracketCosts ys)



-- Task 2

data Term f v = Fun f [Term f v] | Var v deriving (Eq, Show, Ord)

instance (C.Listable f, C.Listable v) => C.Listable (Term f v) where
  tiers  =  C.cons2 Fun C.\/ C.cons1 Var
  
-- naive implementation of the embedding relation, 
-- exponential time requirement
embNaive :: (Eq f, Eq v) => Term f v -> Term f v -> Bool
embNaive (Var x) t = t == Var x
embNaive (Fun f ss) tt@(Fun g ts) 
        | f == g = and (zipWith (\ si tj -> embNaive si tj) ss ts)
           || any (\ si -> embNaive si tt) ss
embNaive (Fun _ ss) tt = any (\ si -> embNaive si tt) ss

-- implement a polynomial time algorithm in the style
-- of the bracketing algorithm, i.e., by storing all
-- required sub-calls in a map or in an array
emb :: (Ord f, Ord v) => Term f v -> Term f v -> Bool
emb s t = m M.! (s, t)
  where
    subterms term@(Var _)    = [term]
    subterms term@(Fun _ ss) = term : concatMap subterms ss

    m = M.fromList [((s', t'), eval s' t') | s' <- subterms s, t' <- subterms t]

    eval (Var x) t = t == Var x
    eval (Fun f ss) tt@(Fun g ts) 
      | f == g = and (zipWith (\si tj -> m M.! (si, tj)) ss ts)
                 || any (\si -> m M.! (si, tt)) ss
    eval (Fun _ ss) tt = any (\si -> m M.! (si, tt)) ss
  
-- testing performance by generating deeply nested terms
singleTestEmb :: Int -> Bool
singleTestEmb n = 
  let gen 0 u = u
      gen i u = Fun "f" [gen (i - 1) u]
      s = gen n (Var "x")
      t = gen (n+1) (Var "x")
  in emb t s && not (emb s t) && emb (gen n (Fun "g" [s])) (gen (2 * n) (Var "x"))
  
-- tests on equivalence and on performance
testsEmb :: IO ()
testsEmb = do
  C.checkFor 1000 ( \ s t -> emb (s :: Term Int Int) t == embNaive s t)
  putStrLn "Testing speed, going from 2 to 40"
  flip mapM_ [2..40]
    (\ i -> if singleTestEmb i 
      then putStrLn $ show i 
      else error $ "singleTestEmb " ++ show i)
