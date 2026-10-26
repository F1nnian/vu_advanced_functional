module Exercise02 (tau, fold, foldInferredType) where

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
constraints =
  let fun t1 t2 = Fun "->" [t1, t2]
      list t = Fun "[]" [t]
      [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, b1, b2, b3, b4, b5, b6, b7] =
        map Var ["a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "b1", "b2", "b3", "b4", "b5", "b6", "b7"]
   in [ (a1, list a9),
        (a2, a5),
        (a3, a6),
        (a4, a5),
        (a1, b2),
        (fun a10 (fun (list a10) (list a10)), fun a7 b1),
        (b1, fun a8 b2),
        (fun a1 (fun a2 (fun a3 a4)), fun a8 b3),
        (b3, fun a5 b4),
        (b4, fun a6 b5),
        (a6, fun a7 b6),
        (b6, fun b5 b7),
        (a4, b7)
      ]

foldTemplateType =
  let fun t1 t2 = Fun "->" [t1, t2]
      [a1, a2, a3, a4] = map Var ["a1", "a2", "a3", "a4"]
   in fun a1 (fun a2 (fun a3 a4))

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

polymorphicLet :: (Bool, String)
polymorphicLet =
  let f = id
   in (f True, f "hello")

polymorphicLambda :: (Bool, String)
polymorphicLambda =
  (\f -> (f True, f "hello")) id

{-
Error:
src/Exercise02.hs:106:20: error: [GHC-25897]
    • Couldn't match expected type ‘t1’ with actual type ‘t1 -> t2’
      ‘t1’ is a rigid type variable bound by
        the inferred type of function :: (t1 -> t2) -> t2
        at src/Exercise02.hs:106:1-20
    • In the first argument of ‘x’, namely ‘x’
      In the expression: x x
      In the expression: \ x -> x x
    • Relevant bindings include
        x :: t1 -> t2 (bound at src/Exercise02.hs:106:13)
        function :: (t1 -> t2) -> t2 (bound at src/Exercise02.hs:106:1)
    |
106 | function = \x -> x x
    |
-}