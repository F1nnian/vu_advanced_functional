{-# LANGUAGE ImportQualifiedPost #-}

module Exercise06_Tseitin where

import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as M
import Data.Monoid
import System.TimeIt (timeIt)

data Formula a
  = Conj [Formula a]
  | Disj [Formula a]
  | Neg (Formula a)
  | Var a
  deriving (Show)

type CnfVar = Integer

type VarMap a = M.Map a CnfVar

type Clause = [CnfVar]

newtype Dimacs = Dimacs ([Clause], Integer)

instance Show Dimacs where
  show (Dimacs (clauses, numVars)) =
    unlines $
      ["p cnf " ++ show numVars ++ " " ++ show (length clauses)]
        ++ map (\c -> unwords (map show c) ++ " 0") clauses

showExtDimacs :: (Show a) => (Dimacs, M.Map a CnfVar) -> String
showExtDimacs (dimacs, mapping) =
  unlines
    ( flip
        map
        (M.toList mapping)
        (\(a, x) -> "c  variable " ++ show a ++ " is encoded as number " ++ show x)
    )
    ++ show dimacs

testFormula = Conj [Neg (Var "c"), Var "a", Disj [Var "a", Neg (Var "1")]]

-- above is the common part for formulas

-- below is the Tseitin transformation based on the RWS Monad

data TseitinState a = TseitinState
  { lastUsedCnfVar :: CnfVar,
    varMap :: VarMap a
  }

nextCnfVar :: (MonadState (TseitinState a) m) => m CnfVar
nextCnfVar = do
  x <- gets lastUsedCnfVar
  let fresh = x + 1
  modify (\s -> s {lastUsedCnfVar = fresh})
  return fresh

lookupVar ::
  (Ord a, MonadState (TseitinState a) m) => a -> m CnfVar
lookupVar x = do
  vmap <- gets varMap
  case M.lookup x vmap of
    Just i -> return i
    Nothing -> do
      i <- nextCnfVar
      let newVmap = M.insert x i vmap
      modify (\s -> s {varMap = newVmap})
      return i

tseitinMain ::
  ( Ord a,
    MonadState (TseitinState a) m,
    MonadWriter MonoidTseitin m
  ) =>
  Formula a -> m CnfVar
tseitinMain (Var x) = lookupVar x
tseitinMain (Neg f) = do
  fi <- tseitinMain f
  j <- nextCnfVar
  addClause [j, fi]
  addClause [-j, -fi]
  return j
tseitinMain (Disj fs) = do
  fis <- mapM tseitinMain fs
  j <- nextCnfVar
  addClause $ -j : fis
  mapM_ (\fi -> addClause [j, -fi]) fis
  return j
tseitinMain (Conj fs) = do
  fis <- mapM tseitinMain fs
  j <- nextCnfVar
  addClause $ j : map (\fi -> -fi) fis
  mapM_ (\fi -> addClause [-j, fi]) fis
  return j

tseitin :: (Ord a) => Formula a -> (Dimacs, M.Map a CnfVar)
tseitin f =
  let init = TseitinState {lastUsedCnfVar = 0, varMap = M.empty}
   in case runRWS (tseitinMain f) () init of
        (fIndex, finalState, clauses) ->
          let allClauses = [fIndex] : monoidToList clauses
              nrVariables = lastUsedCnfVar finalState
              mapping = varMap finalState
           in (Dimacs (allClauses, nrVariables), mapping)

testInvocation = putStrLn $ showExtDimacs $ tseitin $ testFormula

testPerformance n =
  -- just stacking negations
  let f (0 :: Int) = Var "x"; f n = Neg (f (n - 1))
   in timeIt $ print $ length $ showExtDimacs $ tseitin (f n)

--------- YOU ONLY NEED TO CHANGE THE PART BELOW ------------------------

-- Task 2
-- Change the type of MonoidTseitin and corresponding functions,
-- so that a linear time algorithm is obtained.
-- You can use "testPerformance n" with increasing n for testing.

data OpTree a
  = Append (OpTree a) (OpTree a)
  | Singleton a
  | Empty

instance Semigroup (OpTree a) where
  (<>) = Append

instance Monoid (OpTree a) where
  mempty = Empty

{- old implementation, to be changed -}

type MonoidTseitin = OpTree Clause

-- naive version
-- monoidToList :: OpTree a -> [a]
-- monoidToList Empty = []
-- monoidToList (Singleton x) = [x]
-- monoidToList (Append l r) = monoidToList l ++ monoidToList r

monoidToList :: MonoidTseitin -> [Clause]
monoidToList m = go m []
  where
    go Empty acc = acc
    go (Singleton x) acc = x : acc
    go (Append l r) acc = go l (go r acc)

addClause :: (MonadWriter MonoidTseitin m) => Clause -> m ()
addClause c = tell (Singleton c)
