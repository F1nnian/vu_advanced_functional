module Unification(Term(..), mguList, applySubst) where {

import Prelude hiding (foldr);

data Term f v = Var v | Fun f [Term f v] deriving Show;

instance (Eq a, Eq b) => Eq (Term a b) where {
  a == b = equal_term a b;
};

equal_term :: forall a b. (Eq a, Eq b) => Term a b -> Term a b -> Bool;
equal_term (Var x1) (Fun x21 x22) = False;
equal_term (Fun x21 x22) (Var x1) = False;
equal_term (Fun x21 x22) (Fun y21 y22) = x21 == y21 && x22 == y22;
equal_term (Var x1) (Var y1) = x1 == y1;

foldr :: forall a b. (a -> b -> b) -> [a] -> b -> b;
foldr f [] = id;
foldr f (x : xs) = f x . foldr f xs;

fun_upd :: forall a b. (Eq a) => (a -> b) -> a -> b -> a -> b;
fun_upd f a b = (\ x -> (if x == a then b else f x));

subst :: forall a b. (Eq a) => a -> Term b a -> a -> Term b a;
subst x t = fun_upd Var x t;

bind :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b;
bind Nothing f = Nothing;
bind (Just x) f = f x;

eval_term :: forall a b c. (a -> [b] -> b) -> Term a c -> (c -> b) -> b;
eval_term i (Var x) alpha = alpha x;
eval_term i (Fun f ss) alpha = i f (map (\ s -> eval_term i s alpha) ss);

contains_var_term :: forall a b. (Eq a) => a -> Term b a -> Bool;
contains_var_term x (Var y) = x == y;
contains_var_term x (Fun uu ts) = any (contains_var_term x) ts;

subst_list ::
  forall a b.
    (a -> Term b a) -> [(Term b a, Term b a)] -> [(Term b a, Term b a)];
subst_list sigma ys =
  map (\ p -> (eval_term Fun (fst p) sigma, eval_term Fun (snd p) sigma)) ys;

zip_option :: forall a b. [a] -> [b] -> Maybe [(a, b)];
zip_option [] [] = Just [];
zip_option (x : xs) (y : ys) =
  bind (zip_option xs ys) (\ zs -> Just ((x, y) : zs));
zip_option (x : xs) [] = Nothing;
zip_option [] (y : ys) = Nothing;

decompose ::
  forall a b c. (Eq a) => Term a b -> Term a c -> Maybe [(Term a b, Term a c)];
decompose s t =
  (case (s, t) of {
    (Var _, _) -> Nothing;
    (Fun _ _, Var _) -> Nothing;
    (Fun f ss, Fun g ts) -> (if f == g then zip_option ss ts else Nothing);
  });

unify ::
  forall a b.
    (Eq a,
      Eq b) => [(Term a b, Term a b)] ->
                 [(b, Term a b)] -> Maybe [(b, Term a b)];
unify [] bs = Just bs;
unify ((Fun f ss, Fun g ts) : e) bs =
  (case decompose (Fun f ss) (Fun g ts) of {
    Nothing -> Nothing;
    Just us -> unify (us ++ e) bs;
  });
unify ((Var x, t) : e) bs =
  (if equal_term t (Var x) then unify e bs
    else (if contains_var_term x t then Nothing
           else unify (subst_list (subst x t) e) ((x, t) : bs)));
unify ((Fun v va, Var x) : e) bs =
  (if contains_var_term x (Fun v va) then Nothing
    else unify (subst_list (subst x (Fun v va)) e) ((x, Fun v va) : bs));

subst_compose ::
  forall a b c d. (a -> Term b c) -> (c -> Term b d) -> a -> Term b d;
subst_compose sigma tau = (\ x -> eval_term Fun (sigma x) tau);

subst_of :: forall a b. (Eq a) => [(a, Term b a)] -> a -> Term b a;
subst_of ss = foldr (\ (x, t) sigma -> subst_compose sigma (subst x t)) ss Var;

mguList ::
  forall f v. (Eq f, Eq v) => [(Term f v, Term f v)] -> Maybe [(v, Term f v)];
mguList xs = unify xs [];

applySubst :: forall f v. (Eq v) => [(v, Term f v)] -> Term f v -> Term f v;
applySubst sigma t = eval_term Fun t (subst_of sigma);

}

