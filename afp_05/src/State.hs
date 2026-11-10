module State where
-- all functions are public

import Data.Bifunctor

------------ The State Monad ------------------------------
-- Note:
-- this is a direct definition of Haskell's 
-- State type from Control.Monad.State, 
-- without the detour of the StateT type; 
-- the latter will be discussed in some upcoming lecture
        
newtype State s a = State { runState :: s -> (a,s) }

evalState :: State s a -> s -> a
evalState st = fst . runState st

execState :: State s a -> s -> s
execState st = snd . runState st

instance Functor (State s) where
  fmap f st = State ( \ s -> first f $ runState st s )

chainState :: State s a -> (a -> State s b) -> State s b
chainState st1 aSt2 = State 
  (\ s -> let (a, s1) = runState st1 s 
          in runState (aSt2 a) s1)
          
instance Applicative (State s) where
  pure a = State ( \ s -> (a,s))
  liftA2 f st1 st2 = chainState st1 (\ x -> fmap (f x) $ st2)
  
instance Monad (State s) where
  return = pure
  (>>=) = chainState
  
get :: State s s 
get = State ( \ s -> (s, s))

put :: s -> State s ()
put s = State ( \ _ -> ((), s))
