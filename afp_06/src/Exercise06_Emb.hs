{-# LANGUAGE ImportQualifiedPost #-}

module Exercise06_Emb where

import Control.Monad
import Control.Monad.ST (runST)
import Control.Monad.State
import Data.Map qualified as M
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import System.TimeIt (timeIt)

data Term f v = Var v | Fun f [Term f v] deriving (Eq, Ord, Show)

type LTerm f v = Term (Int, f) (Int, v)

labelTerm :: Term f v -> LTerm f v
labelTerm t = evalState (lt t) 0
  where
    getIndex :: State Int Int
    getIndex = get >>= \i -> put (i + 1) >> return i

    lt :: Term f v -> State Int (LTerm f v)
    lt (Var v) = (Var . flip (,) v) <$> getIndex
    lt (Fun f ts) = do
      i <- getIndex
      Fun (i, f) <$> mapM lt ts

embState :: (Ord f, Ord v) => Term f v -> Term f v -> (Bool, Int)
embState s t =
  let look s t = M.lookup (s, t) <$> get
      store s t b = (M.insert (s, t) b <$> get) >>= put
      (res, m) = runState (embMain look store s t) M.empty
   in (res, M.size m)

embST :: (Ord f, Ord v) => Term f v -> Term f v -> (Bool, Int)
embST s t =
  runST
    ( do
        mRef <- newSTRef M.empty
        let look s t = M.lookup (s, t) <$> readSTRef mRef
        let store s t b = (M.insert (s, t) b <$> readSTRef mRef) >>= writeSTRef mRef
        res <- embMain look store s t
        m <- readSTRef mRef
        return (res, M.size m)
    )

data Alg = AlgST | AlgState | AlgLabelState

data Output = NoSize | WithSize

testAlg :: (Ord f, Ord v) => Alg -> Term f v -> Term f v -> (Bool, Int)
testAlg AlgST = embST
testAlg AlgState = embState
testAlg AlgLabelState = embLabelState

singleTestEmb :: Alg -> Int -> IO (Bool, Int)
singleTestEmb alg n =
  let emb = \s t -> return (testAlg alg s t)
      gen 0 u = u
      gen i u = Fun "f" [gen (i - 1) u]
      genVar i = Var $ "x" ++ show i
      m = 100 * n
      s' = Fun "f" $ map genVar [0 .. m]
      t' = genVar (m `div` 3)
      s = gen n (Var "x")
      t = gen (n + 1) (Var "x")
   in do
        (ts, c1) <- emb t s
        (st, c2) <- emb s t
        (fg, c3) <- emb (gen n (Fun "g" [s])) (gen (2 * n) (Var "x"))
        (ff, c4) <- emb (gen (2 * n) (Var "x")) (gen n (Var "x"))
        (prime, c5) <- emb s' t'
        let c = c1 + c2 + c3 + c4 + c5
        return (ts && not st && fg && ff && prime, c)

runTest :: Alg -> Output -> Int -> IO ()
runTest alg out n =
  timeIt
    ( do
        (b, size) <- singleTestEmb alg n
        putStrLn $ "Result is correct: " ++ show b
        case out of
          NoSize -> putStrLn "No size requested"
          WithSize -> putStrLn $ "Size of map: " ++ show size
    )

sep = "\n------------------------------\n"

putInfo name = putStr $ sep ++ name ++ sep

runTests :: Int -> IO ()
runTests n = do
  putInfo "ST with size"
  runTest AlgST WithSize n
  putInfo "ST without size"
  runTest AlgST NoSize n
  putInfo "State with size"
  runTest AlgState WithSize n
  putInfo "State without size"
  runTest AlgState NoSize n
  putInfo "State(Label) with size"
  runTest AlgLabelState WithSize n
  putInfo "State(Label) without size"
  runTest AlgLabelState NoSize n

------- DON'T CHANGE THE CODE ABOVE (except for imports) ------------------

{-
  Task 1.A

  change embMain, allM, anyM so that

  runTest (AlgST or AlgState) (WithSize or NoSize) n

  roughly need the same execution time (same order of magnitude).

-}

embMain ::
  (Eq f, Eq v, Monad m) =>
  (Term f v -> Term f v -> m (Maybe Bool)) -> -- lookup
  (Term f v -> Term f v -> Bool -> m ()) -> -- store
  Term f v ->
  Term f v ->
  m Bool
embMain = embMainWith (==) (==)

embMainWith ::
  (Monad m) =>
  (f -> f -> Bool) ->
  (v -> v -> Bool) ->
  (Term f v -> Term f v -> m (Maybe Bool)) -> -- lookup
  (Term f v -> Term f v -> Bool -> m ()) -> -- store
  Term f v ->
  Term f v ->
  m Bool
embMainWith eqF eqV look store = main
  where
    main s t = do
      maybeResult <- look s t
      case maybeResult of
        Just b -> return b
        Nothing -> do
          result <- main2 s t
          store s t result
          return result
    main2 (Var x) (Var y) = return $ eqV x y
    main2 (Var _) _ = return False
    main2 (Fun f ss) t@(Fun g ts)
      | eqF f g = do
          bigConj <- allM (\(si, ti) -> main si ti) (zip ss ts)
          if bigConj then return True else anyM (\si -> main si t) ss
    main2 (Fun _ ss) t = anyM (\si -> main si t) ss

allM, anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM f (x : xs) = do
  b <- f x
  if b then allM f xs else return False
anyM _ [] = return False
anyM f (x : xs) = do
  b <- f x
  if b then return True else anyM f xs

{-
  Task 1.B

  It might be necessary to adjust embMain.

  You can run performance tests by "runTests n"
  Note that the printed Boolean should always be True.
-}

labelOf :: LTerm f v -> Int
labelOf (Var (i, _)) = i
labelOf (Fun (i, _) _) = i

embLabelState :: (Eq f, Eq v) => Term f v -> Term f v -> (Bool, Int)
embLabelState s t =
  let eqSnd (_, a) (_, b) = a == b
      look s' t' = M.lookup (labelOf s', labelOf t') <$> get
      store s' t' b = (M.insert (labelOf s', labelOf t') b <$> get) >>= put
      (res, m) = runState (embMainWith eqSnd eqSnd look store (labelTerm s) (labelTerm t)) M.empty
   in (res, M.size m)
