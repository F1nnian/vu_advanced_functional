module Demo06_Emb where

import Data.STRef(writeSTRef,readSTRef,newSTRef)
import Control.Monad.ST(runST)

import Control.Monad.State
import Control.Monad

import System.TimeIt(timeIt)

import qualified Data.Map as M


g b = putStrLn (show b) >> return b

f mb1 mb2 = do
  b1 <- mb1
  b2 <- mb2
  return $ b1 || b2
  
h m1 m2 m3 = do
  x <- m1
  y <- m2
  z <- m3
  return (x, y, z)
  
test1 = let xs = Just [1..100 :: Int] in
  h xs xs xs

test2 = let xs = [1..100 :: Int] in
  h xs xs xs

data Term f v = Var v | Fun f [Term f v] deriving (Eq, Ord, Show)

embMain :: (Eq f, Eq v, Monad m) => 
  (Term f v -> Term f v -> m (Maybe Bool)) -- lookup
  -> (Term f v -> Term f v -> Bool -> m ()) -- store
  -> Term f v -> Term f v -> m Bool 
embMain look store = main where
  main s t = do 
    maybeResult <- look s t
    case maybeResult of
      Just b -> return b
      Nothing -> do 
        result <- main2 s t
        store s t result 
        return result
  main2 (Var x) t = return $ t == Var x
  main2 (Fun f ss) t@(Fun g ts) 
   | f == g = do
      bigConj <- allM ( \ (si,ti) -> main si ti) (zip ss ts)
      bigDisj <- anyM ( \ si -> main si t) ss
      return $ bigConj || bigDisj
  main2 (Fun f ss) t = anyM ( \ si -> main si t) ss


allM, anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f xs = and <$> mapM f xs
anyM f xs = foldM (\ b x -> (b ||) <$> f x) False xs


embState :: (Ord f, Ord v) => Term f v -> Term f v -> (Bool, Int)
embState s t = let
    look s t = M.lookup (s,t) <$> get
    store s t b = (M.insert (s,t) b <$> get) >>= put
    (res, m) = runState (embMain look store s t) M.empty
  in (res, M.size m) 

embST :: (Ord f, Ord v) => Term f v -> Term f v -> (Bool, Int)
embST s t = runST (do
  mRef <- newSTRef M.empty
  let look s t = M.lookup (s,t) <$> readSTRef mRef
  let store s t b = (M.insert (s,t) b <$> readSTRef mRef) >>= writeSTRef mRef 
  res <- embMain look store s t
  m <- readSTRef mRef
  return (res, M.size m))

data Alg = AlgST | AlgState

data Output = NoSize | WithSize

testAlg :: (Ord f, Ord v) => Alg -> Term f v -> Term f v -> IO (Bool, Int)
testAlg AlgST = \ s t -> return $ embST s t
testAlg AlgState = \ s t -> return $ embState s t

singleTestEmb :: Alg -> Int -> IO (Bool, Int)
singleTestEmb alg n =
  let emb = testAlg alg
      gen 0 u = u
      gen i u = Fun "f" [gen (i - 1) u]
      genVar i = Var $ "x" ++ show i
      m = 100 * n
      s' = Fun "f" $ map genVar [0..m]
      t' = genVar (m `div` 3)
      s = gen n (Var "x")
      t = gen (n+1) (Var "x")
  in do
      (ts, c1) <- emb t s
      (st, c2) <- emb s t
      (fg, c3) <- emb (gen n (Fun "g" [s])) (gen (2 * n) (Var "x"))
      (ff, c4) <- emb (gen (2 * n) (Var "x")) (gen n (Var "x"))
      (prime, c5) <- emb s' t'
      let c = c1 + c2 + c3 + c4 + c5
      return (ts && not st && fg && ff && prime, c)
      
runTest :: Alg -> Output -> Int -> IO ()
runTest alg out n = timeIt $ do
  (b,size) <- singleTestEmb alg n
  putStrLn $ "Result: " ++ show b
  case out of 
    NoSize -> putStrLn "No size requested"
    WithSize -> putStrLn $ "Size of map: " ++ show size
    
runTests :: Int -> IO ()
runTests n = do
  putStrLn "ST - No-Size"
  runTest AlgST NoSize n
  putStrLn "-----------------"
  putStrLn "ST - With-Size"
  runTest AlgST WithSize n
  putStrLn "-----------------"
  putStrLn "State - No-Size"
  runTest AlgState NoSize n
  putStrLn "-----------------"  
  putStrLn "State - With-Size"
  runTest AlgState WithSize n
