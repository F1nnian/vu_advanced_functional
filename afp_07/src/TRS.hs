module TRS where

type Metadata = [(String,String)]
type Id = String
type Signature = [(Id,Int)]
data Term = Var Id | Fun Id [Term] 
type Rule = (Term,Term)
type TRS = [Rule]

instance Show Term where
  show (Var x) = x
  show (Fun f []) = f 
  show (Fun f (t : ts)) = f ++ "(" ++ show t ++ concatMap (\ ti -> "," ++ show ti) ts ++ ")"