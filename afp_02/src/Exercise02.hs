module Exercise02(tau, fold, foldInferredType) where

{-
The given unification algorithm slighty deviates from the lecture:
main property: 
if 

     mguList unifProblem = Just mgu

then 

     applySubst mgu t

applies the mgu on a term t and

     mgu is a representation of an mgu of unifProblem
     (this representation differs from the lecture)
-}
import Unification

-- Task 1

{-

(a) construct the constraints for typing the following variant of a fold-function

-}

fold [] e f = e
fold (x : xs) e f = f x (fold xs e f)

{-
  on paper

(b) encode these constraints as a unification problem,
    invoke the unification algorithm and compute the most general type of fold
 

    hint:

    let fun t1 t2 = Fun "->" [t1, t2]
        [a1, a2, a3] = map Var ["a1", "a2", "a3"]
    in fun a1 (fun a2 a3)

    constructs the type "a1 -> a2 -> a3" as a term to be used in the unification algorithm
-}

constraints :: [(Term String String, Term String String)]
constraints = undefined
foldTemplateType = undefined -- encoding of "a1 -> a2 -> a3 -> a4"

tau = let Just mgu = mguList constraints in mgu -- unification should succeed!

foldInferredType = applySubst tau foldTemplateType -- type of fold



{- Task 2

- explain handling of type-inference of lambda-abstractions,
  and generate constraints for
  
function = \ x -> x x

- consider difference of 

polymorphicLet :: (Bool, String)
polymorphicLet =
  let f = id
  in (f True, f "hello")

polymorphicLambda :: (Bool, String)
polymorphicLambda =
  (\ f -> (f True, f "hello")) id

-}