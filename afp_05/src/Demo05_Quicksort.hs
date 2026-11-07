module Demo05_Quicksort where

import Data.List(partition)
import Data.IORef(writeIORef,readIORef,newIORef)
import Data.STRef(STRef, writeSTRef,readSTRef,newSTRef)
import System.Random
import Control.Monad.ST(ST, runST)
import System.TimeIt(timeIt)

-- choose between lecture implementation or predefined State monad
import State
-- import Control.Monad.State

----- pure code, non-randomized quick-sort

qsortCount :: Ord a => [a] -> (Integer, [a])
qsortCount [] = (0,[])
qsortCount (x : xs) = let
  (low, high) = partition (< x) xs
  c0 = fromIntegral $ length xs
  (c1, qs1) = qsortCount low
  (c2, qs2) = qsortCount high
 in (c0 + c1 + c2, qs1 ++ [x] ++ qs2)

testList :: [Int]
testList = [1..12345]

testQSDet = timeIt (do
  let (qsCount, sortedList) = qsortCount testList
  putStrLn $ "sorted list of length: " ++ show (length sortedList)
  putStrLn $ "comparisons: " ++ show qsCount)

--- randomized quicksort, using monads

-- rng n should provide random number in 0 .. n
qsortMonadic :: (Monad m, Ord a) => (Int -> m Int) -> [a] -> m (Integer,[a])
qsortMonadic rng = qsortMain where 
  qsortMain [] = return $ (0, [])
  qsortMain xs = do 
     pos <- rng $ n - 1
     let (xs1, x : xs2) = splitAt pos xs
     let (low, high) = partition (< x) (xs1 ++ xs2)
     let c0 = fromIntegral $ n - 1
     (c1,qs1) <- qsortMain low
     (c2,qs2) <- qsortMain high
     return $ (c0 + c1 + c2, qs1 ++ [x] ++ qs2)
    where n = length xs

-- IO implementation using built-in rng functionality
-- randomRIO :: (Random a, UniformRange a) => (a, a) -> IO a
-- randomRIO :: (Int, Int) -> IO Int

-- return number between 0 and n
getRandomIO :: Int -> IO Int
getRandomIO n = randomRIO (0, n)

qsortIO :: Ord a => [a] -> IO (Integer,[a])
qsortIO = qsortMonadic getRandomIO

testQSIO :: [Int] -> IO ()
testQSIO = mapM_ (\_ -> qsortIO testList >>= \ r -> print (fst r))

----- now without IO, using State monad and pseudo rng

type RandomState = State StdGen

getRandomState :: Int -> RandomState Int
getRandomState n = do
  rng <- get
  let (r, rng') = uniformR (0,n) rng
  put rng'
  return r

qsortStateMain :: Ord a => [a] -> RandomState (Integer, [a])
qsortStateMain = qsortMonadic getRandomState

qsortState :: Ord a => Int -> [a] -> (Integer,[a])
qsortState seed xs = evalState (qsortStateMain xs) (mkStdGen seed)

testQSState :: [Int] -> IO ()
testQSState seeds = mapM_ (\ seed -> 
  return (qsortState seed testList) >>= \ r -> print (fst r)) seeds
  
----- now using IO with pseudo rng, which is stored via IO references

qsortIOSeed :: Ord a => Int -> [a] -> IO (Integer, [a])
qsortIOSeed seed xs = do
  rngRef <- newIORef (mkStdGen seed) 
  let getRandIO n = do 
       rng <- readIORef rngRef
       let (r, rng') = uniformR (0,n) rng
       writeIORef rngRef rng'
       return r
  qsortMonadic getRandIO xs

testQSIOSeed :: [Int] -> IO ()
testQSIOSeed seeds = mapM_ (\ seed -> 
  qsortIOSeed seed testList >>= \ r -> print (fst r)) seeds
  
----- now using ST with pseudo rng

qsortMonadicST :: Ord a => Int -> [a] -> ST s (Integer, [a])
qsortMonadicST seed xs = do
  rngRef <- newSTRef (mkStdGen seed) 
  let getRandST n = do 
       rng <- readSTRef rngRef
       let (r, rng') = uniformR (0,n) rng
       writeSTRef rngRef rng'
       return r
  qsortMonadic getRandST xs
    
qsortSTSeed :: Ord a => Int -> [a] -> (Integer, [a])
qsortSTSeed seed xs = runST $ qsortMonadicST seed xs

testQSSTSeed :: [Int] -> IO ()
testQSSTSeed seeds = mapM_ (\ seed -> 
  return (qsortSTSeed seed testList) >>= \ r -> print (fst r)) seeds
  
tests = do 
  let seeds = [1..10]
  putStrLn "State monad"
  timeIt (testQSState seeds)
  putStrLn "-------------------------"
  putStrLn "ST monad"
  timeIt (testQSSTSeed seeds)
  putStrLn "-------------------------"
  putStrLn "IO monad (IO seed)"
  timeIt (testQSIOSeed seeds)
  putStrLn "-------------------------"
  putStrLn "IO monad (random IO)"
  timeIt (testQSIO seeds)

-- bad = let r = runST (do { r <- newSTRef True; writeSTRef r False; return r })
--       in  runST (readSTRef r)