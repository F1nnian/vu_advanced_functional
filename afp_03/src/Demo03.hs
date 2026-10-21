module Demo03 where

import Prelude hiding(log)
import Data.List
import Data.Functor.Const


twice :: a -> [a]
twice x = [x, x]

test1 = (twice True, twice (twice (5 :: Integer)))
test2 = let twiceLocal x = [x, x] in (twiceLocal True, twiceLocal 'c')
{-
createGen1 p = (p True, p 'c')       -- not allowed
createGen2 = \ p -> (p True, p 'c')  -- not allowed
test3 = createGen1 twice
test4 = createGen2 twice
-}

-- test in ghci: 
--   drop explicit type of typingTest, and run ":t typingTest" to see inferred type
typingTest :: (Eq a, Num a, Ord b) => a -> b -> b -> b
typingTest 0 x c = x
typingTest n x y = if typingTest (n - 1) True (x > y) 
  then y 
  else typingTest (n - 1) y x


f x = if x == (0 :: Int) then g h else g (g (f (x - 1)))
h = g (f 5)
g x = x . x
j c = if c == 'a' then 'z' else i c
i c = pred c

data Ty a b = Con (a b)

value :: Ty Maybe Int
value = Con (Just 5)

sortRev :: forall a. Ord a => [a] -> [(a, a)]
sortRev xs = zip sorted reversed where
  sorted :: [a]
  sorted = sort xs
  reversed :: [a]
  reversed = reverse xs


appl :: (forall a. Show a => a -> IO ()) -> IO () 
appl log = do
  s <- readFile "input_03.txt"
  let [user, iban, balanceS] = (lines s :: [String])
  let balance = (read balanceS :: Double)
  log user
  log balance
  if balance > 1000000 then print $ user ++ " is rich" else return ()
  
noLog x = return ()
logToFile file x = appendFile file (show x ++ "\n")
logStdOut x = putStrLn $ "log: " ++ show x

main1, main2, main3 :: IO ()
main1 = appl noLog 
main2 = appl (logToFile "log.txt")
main3 = appl logStdOut

main :: IO ()
main = main1


createGen1, createGen2 :: (forall a. a -> b a) -> (b Bool, b Char)
createGen1 p = (p True, p 'c')
createGen2 = \ p -> (p True, p 'c')

testList1, testList2 :: ([Bool], [Char]) -- b = []
testList1 = createGen1 (\ x -> [x, x])
testList2 = createGen2 (\ x -> [x, x])

testMaybe :: (Maybe Bool, Maybe Char) -- b = Maybe
testMaybe = createGen1 Just

number :: a -> Const Int a
number x = Const 42

testInt :: (Const Int Bool, Const Int Char)
testInt = createGen1 number


