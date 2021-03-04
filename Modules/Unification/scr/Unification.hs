module Unification where

import Type

import Substitutions
import SetsAsOrderedList
import Variables

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var a)     (Var b)     = if a == b then Nothing else Just (Var a,Var b)
ds (Var a)     (Comb b lB) = Just (Var a, Comb b lB)
ds (Comb a lA) (Var b)     = Just (Var b, Comb a lA)
ds (Comb a lA) (Comb b lB) = if (a == b) && (length lA == length lB)
                               then f Nothing lA lB
                               else Just (Comb a lA, Comb b lB)
 where
  f :: Maybe (Term, Term) -> [Term] -> [Term] -> Maybe (Term, Term)
  f Nothing (a:la) (b:lb) = f (ds a b) la lb
  f Nothing []    _       = Nothing
  f Nothing _     []      = Nothing
  f x       _     _       = x



unify :: Term -> Term -> Maybe Subst
unify a b = f1 Substitutions.empty a b -- launch the unification with empty substitutuin
    where 
        f1 :: Subst -> Term -> Term -> Maybe Subst
        f1 s a b = if (apply s a) == (apply s b)
                     then Just s
                     else f2 (ds (apply s a) (apply s b)) s (apply s a) (apply s b) 
        f2 :: Maybe (Term,Term) -> Subst -> Term -> Term -> Maybe Subst
        f2 (Just (Var un, ut) ) s t1 t2 = if isElem un (allVarsSet ut)
                                            then Nothing  -- Fail
                                            else f1 (compose (single un ut) s) t1 t2  -- next step
        f2 (Just (Comb _ _, Comb _ _)) _ _  _  = Nothing  -- Fail
        f2 Nothing _ _  _                      = Nothing  -- Big Fail (error in the code)



{-

"ds f(g,$1)  f($2,e) -> ($1,$2)"


$2 -> g, $1 -> e

{$1->$2,...}
{_ -> _, A -> A}

-}



