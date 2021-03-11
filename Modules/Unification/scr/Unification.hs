module Unification 
  ( ds
  , unify
  )
  where

import Type

import Substitutions
import SetsAsOrderedList
import Variables

-- 1.
-- \ Determines the Disagreement Set of two terms, being the two first 
--  different entries found when traversing through two terms
--  from left to right.
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var v0)      (Var  v1)     = if v0 == v1 then Nothing else Just (Var v0,Var v1)
ds (Var v0)      (Comb c1 ts1) = Just (Var v0, Comb c1 ts1)
ds (Comb c0 ts0) (Var v1)      = Just (Var v1, Comb c0 ts0)
ds (Comb c0 ts0) (Comb c1 ts1) = if (c0 == c1) && (length ts0 == length ts1)
                                 then dsList ts0 ts1
                                 else Just (Comb c0 ts0, Comb c1 ts1)
 where
  dsList ::[Term] -> [Term] -> Maybe (Term, Term)
  dsList (a:as) (b:bs) = case ds a b of
    Nothing -> dsList as bs
    just    -> just
  dsList _        _      = Nothing


-- 2.
-- \ Determines a Most Common Unifyer of two terms if existent.
unify :: Term -> Term -> Maybe Subst   
unify t0 t1 = if t0 == t1
                then Just Substitutions.empty
                else case ds t0 t1 of
                  -- If the disagreement set contains a variable do some recursion.
                  Just (Var vName, t) -> do
                    occurCheck vName t  -- Nothing results in Fail
                    newSub  <- Just (single vName t)
                    restSub <- unify (apply newSub t0) (apply newSub t1)
                    return (compose newSub restSub)
                  -- otherwise fail
                  _                   -> Nothing
 where
  -- \ Some guard to check wether a variable is contained in a term.
  occurCheck :: VarName -> Term -> Maybe ()
  occurCheck v t = if isElem v (allVarsSet t)
                     then Nothing  -- Fail
                     else Just ()  -- no fail

unify2 :: Term -> Term -> Maybe Subst
unify2 t0 t1 = if t0 == t1
                  then Just Substitutions.empty
                  else case ds 


t1 = Comb "=" [Var (VarName "X"),Var (VarName "X")]
t2 = Comb "=" [Var (VarName "A"),Var (VarName "B")]

-- unify (=(X,X)) (=(A,B))
-- {X -> A, A -> B}
-- =(X,X)
-- =(A,B)

-- =(A,A)
-- =(B,B)


