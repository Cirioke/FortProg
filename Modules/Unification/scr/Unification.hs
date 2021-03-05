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
ds (Var v0)     (Var  v1)    = if v0 == v1 then Nothing else Just (Var v0,Var v1)
ds (Var v0)     (Comb c0 ts) = Just (Var v0, Comb b lB)
ds (Comb c0 lA) (Var v1)     = Just (Var b, Comb a lA)
ds (Comb a lA)  (Comb b lB)  = if (a == b) && (length lA == length lB)
                                 then dsList lA lB
                                 else Just (Comb a lA, Comb b lB)
 where
  dsList ::[Term] -> [Term] -> Maybe (Term, Term)
  dsList (_a:la) (_b:lb) = case ds _a _b of
    Nothing -> f la lb
    just    -> just
  dsList _        _      = Nothing


-- 2.
-- \ Determines a most common unifyer of two terms if existent.
unify :: Term -> Term -> Maybe Subst
unify a b = f1 Substitutions.empty a b -- launch the unification with empty substitutuin
    where 
        f1 :: Subst -> Term -> Term -> Maybe Subst
        f1 s _a _b = if (apply s _a) == (apply s _b)
                     then Just s
                     else f2 (ds (apply s _a) (apply s _b)) s (apply s _a) (apply s _b) 
        f2 :: Maybe (Term,Term) -> Subst -> Term -> Term -> Maybe Subst
        f2 (Just (Var un, ut) ) s t1 t2 = if isElem un (allVarsSet ut)
                                            then Nothing  -- Fail
                                            else f1 (compose (single un ut) s) t1 t2  -- next step
        f2 (Just (Comb _ _, Comb _ _)) _ _  _  = Nothing  -- Fail
        f2 Nothing _ _  _                      = Nothing  -- Big Fail (error in the code)
        f2 _       _ _  _                      = Nothing  -- Big Fail (error in the code)

-- \ Determines a most common unifyer of two terms if existent.
-- unify' :: Term -> Term -> Maybe Subst
-- unify' = _unify Substitutions.empty
--   where
--     -- \ Some guard to check wether a variable is contained in a term.
--     occurCheck :: VarName -> Term -> Maybe ()
--     occurCheck v t = if isElem v (allVarsSet t)
--                        then Nothing  -- Fail
--                        else Just ()  -- no fail

--     -- \ Unifications with accumulator argument for the 
--     _unify :: Subst -> Term -> Term -> Maybe Subst
--     _unify sub _t0 _t1 =
--       if t0 == t1 
--         then (Just sub)
--         else case ds t0 t1 of 
--           -- If the disagreement set contains a variable do some recursion.
--           Just (Var vName, t) -> do
--             occurCheck vName t
--             return (compose sub (single vName t)) t0 t1)
--           -- otherwise fail
--           _                   -> Nothing
--       where
--         t0 = apply sub _t0
--         t1 = apply sub _t1


 -- \ Some guard to check wether a variable is contained in a term.
occurCheck :: VarName -> Term -> Maybe ()
occurCheck v t = if isElem v (allVarsSet t)
                    then Nothing  -- Fail
                    else Just ()  -- no fail

-- \ Determines a Most Common Unifyer of two terms if existent.
unify :: Term -> Term -> Maybe Subst
unify t0 t1 = if t0 == t1
                then Just Substitutions.empty
                else case ds t0 t1 of 
                  -- If the disagreement set contains a variable do some recursion.
                  Just (Var vName, t) -> do
                    occurCheck vName t
                    newSub  <- single <$> vName <*> t)
                    restSub <- unify (apply newSub t0) (apply newSub t1)
                    return (compose newSub restSub)
                  -- otherwise fail
                  _                   -> Nothing
   
        




