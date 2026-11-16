module Demo06_Tseitin_Monad_RWS where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import qualified Data.Map as M

data Formula a = 
   Conj [Formula a]
 | Disj [Formula a]
 | Neg (Formula a)
 | Var a
  deriving Show

type CnfVar = Integer
type VarMap a = M.Map a CnfVar

type Clause = [CnfVar]
newtype Dimacs = Dimacs ([Clause], Integer)

instance Show Dimacs where
  show (Dimacs (clauses, numVars)) = unlines $
   ["p cnf " ++ show numVars ++ " " ++ show (length clauses)]
     ++ map ( \ c -> unwords (map show c) ++ " 0") clauses

showExtDimacs :: Show a => (Dimacs, M.Map a CnfVar) -> String
showExtDimacs (dimacs, mapping) = 
  unlines (flip map (M.toList mapping) 
      (\ (a, x) -> "c  variable " ++ show a ++ " is encoded as number " ++ show x))
    ++ show dimacs

testFormula = Conj [Neg (Var "c"), Var "a", Disj [Var "a", Neg (Var "1")]]


-- above is common part for formulas
-- below is the Tseitin transformation based on monad transformers 

data TseitinState a = TseitinState {
     lastUsedCnfVar :: CnfVar,
     varMap :: VarMap a     
  }


nextCnfVar :: MonadState (TseitinState a) m => m CnfVar
nextCnfVar = do 
  x <- gets lastUsedCnfVar
  let fresh = x + 1
  modify (\ s -> s { lastUsedCnfVar = fresh })
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
       modify (\ s -> s { varMap = newVmap })
       return i

addClause :: MonadWriter [Clause] m => Clause -> m ()
addClause c = tell [c]

tseitinMain :: (Ord a, 
  MonadState (TseitinState a) m,
  MonadWriter [Clause] m) => 
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
   addClause $ -j :  fis
   mapM_ (\ fi -> addClause [- fi, j]) fis
   return j
tseitinMain (Conj fs) = do
   fis <- mapM tseitinMain fs
   j <- nextCnfVar
   addClause $ j : map (\ fi -> - fi) fis
   mapM_ (\ fi -> addClause [-j, fi]) fis
   return j

tseitinMainRWS :: Ord a => Formula a -> RWS () [Clause] (TseitinState a) CnfVar
tseitinMainRWS = tseitinMain 

-- running the monad
tseitin :: Ord a => Formula a -> (Dimacs, M.Map a CnfVar)
tseitin f = 
  let init = TseitinState {lastUsedCnfVar = 0, varMap = M.empty}
  in case runRWS (tseitinMain f) () init of
    (fIndex, finalState, clauses) -> 
       let allClauses = [fIndex] : clauses
           nrVariables = lastUsedCnfVar finalState
           mapping = varMap finalState
         in (Dimacs (allClauses, nrVariables), mapping)

testInvocation = putStrLn $ showExtDimacs $ tseitin $ testFormula