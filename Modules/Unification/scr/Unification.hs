module Unification where

import Type

import Substitutions

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var a)     (Var b)     = if a == b then Nothing else Just (Var a,Var b)
ds (Var a)     (Comb b lB) = Just (Var a, Comb b lB)
ds (Comb a lA) (Var b)     = Just (Var b, Comb a lA)
ds (Comb a lA) (Comb b lB) = if a == b
                               then f Nothing lA lB
                               else Just (Comb a lA, Comb b lB)
 where
  f :: Maybe (Term, Term) -> [Term] -> [Term] -> Maybe (Term, Term)
  f Nothing (a:la) (b:lb) = f (ds a b) la lb
  f Nothing []    _       = Nothing
  f Nothing _     []      = Nothing
  f x       _     _       = x


unify :: Term -> Term -> Maybe Subst
unify a b 

{-
"ds f(g,$1)  f($2,e) -> ($1,$2)"


$2 -> g, $1 -> e

{$1->$2,...}
{_ -> _, A -> A}-}



