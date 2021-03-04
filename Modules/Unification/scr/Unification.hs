module Unification where

import Type

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var a)     (Var b)     = if a == b then Nothing else (a,b)
ds (Var a)     (Comb b)    = (Var a, Comb b)
ds (Comb a l)  (Var b)     = (Var b, Comb a)
ds (Comb a lA) (Comb b lB) = if a == b
                               then f Nothing lA lB
                               else: (Comb a lA, Comb b lB)
 where
  f :: [Term] -> [Term] -> Maybe (Term, Term)
  f Nothing (a:la) (b:lb) = f


