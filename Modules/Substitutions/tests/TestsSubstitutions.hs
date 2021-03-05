{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Type

import Variables
import Substitutions

-- \ Some helper function to treat lists like sets,
-- is_subs determines wether all elements of one list are in anotherone.
is_subs :: Eq a => [a] -> [a] -> Bool
is_subs []     _   = True
is_subs (x:xs) lst = (x `elem` lst) &&  (xs `is_subs` lst)


-- \ Some helper function to treat lists like sets,
-- is_set_eq determines wether all elements of one list are in anotherone
-- and the other way around.
is_set_eq :: Eq a => [a] -> [a] -> Bool
is_set_eq l0 l1 = (l0 `is_subs` l1) && (l1 `is_subs` l0)


-- ∀t:apply(empty,t) = t
prop_empty_neutral :: Term -> Bool
prop_empty_neutral t = apply empty t == t

-- ∀x,t:apply(single(x,t),Var x) = t
prop_one_var_subst :: VarName -> Term -> Bool
prop_one_var_subst x t = apply (single x t) (Var x) == t

-- ∀t,s1,s2:apply(compose(s1,s2),t) = apply(s1,apply(s2,t)) 
prop_compose_by_def :: Term -> Subst -> Subst -> Bool
prop_compose_by_def t s1 s2 = apply (compose s1 s2) t 
                              == 
                              apply s1 (apply s2 t)

-- domain(empty) = {}
prop_empty_domain_empty :: Bool
prop_empty_domain_empty = domain empty == []

-- ∀x:domain(single(x,Var x)) = {}
prop_domain_ignores_selfimages :: VarName -> Bool
prop_domain_ignores_selfimages x = domain (single x (Var x)) == []

-- ∀x,t:t ≠ Var x ⇒ domain(single(x,t)) = {x}
prop_domain_recognizes_notselfimages :: VarName -> Term -> Property
prop_domain_recognizes_notselfimages x t = Var x /= t 
                                           ==> 
                                           domain (single x t) == [x]


-- ∀s1,s2:domain(compose(s1,s2)) ⊆ domain(s1) ∪ domain(s2)
prop_comp_dom_subset_united_doms :: Subst -> Subst -> Bool
prop_comp_dom_subset_united_doms s1 s2 = 
  (domain (compose s1 s2))
  `is_subs`
  (domain s1 ++ (domain s2))

-- ∀x1,x2:x1 ≠ x2 
--        ⇒ domain(compose(single(x2,Var x1),single(x1,Var x2))) = {x2}
prop_domain_0 :: VarName -> VarName -> Property
prop_domain_0 x1 x2 = 
  x1 /= x2 
  ==>
  domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]
                                 
-- allVars(empty) = {}
prop_empty_vars_empty :: Bool
prop_empty_vars_empty = allVars empty == []

-- ∀x:allVars(single(x,Var x)) = {}
prop_allVars_ignores_selfimages :: VarName -> Bool
prop_allVars_ignores_selfimages x = 
  allVars (single x (Var x)) == []

-- ∀x,t:t ≠ Var x ⇒ allVars(single(x,t)) = allVars(t) ∪ {x}
prop_single_vars_eq_term_vars_and_varName :: VarName -> Term -> Property
prop_single_vars_eq_term_vars_and_varName x t = 
  (t /= Var x)
  ==> 
  (singleVars `is_set_eq` unionVars)
  where 
    singleVars = allVars (single x t)
    unionVars  = (allVars t) ++ [x]

-- ∀s1,s2:allVars(compose(s1,s2)) ⊆ allVars(s1) ∪ allVars(s2)
prop_comp_vars_subset_united_vars:: Subst -> Subst -> Bool
prop_comp_vars_subset_united_vars s1 s2 =
  (allVars (compose s1 s2))
  `is_subs`
  (allVars s1 ++ allVars s2)


-- ∀x1,x2:x1 ≠ x2 ⇒ allVars(compose(single(x2,Var x1),single(x1,Var x2)))
--                   = {x1,x2}
prop_allVars_0 :: VarName -> VarName -> Property
prop_allVars_0 x1 x2 = 
  x1 /= x2 
  ==>
  (allVars (compose (single x2 (Var x1)) (single x1 (Var x2))) )
  `is_set_eq`
  [x1, x2]

-- ∀s:domain(s) ⊆ allVars(s)
prop_dom_subset_vars:: Subst -> Bool
prop_dom_subset_vars s = (domain s) `is_subs` (allVars s)

-- ∀xs:domain(restrictTo(empty,xs)) = {}
prop_restrictTo_empty_leaves_empty_domain :: [VarName] -> Bool
prop_restrictTo_empty_leaves_empty_domain xs = 
  domain (restrictTo empty xs) == []

-- ∀xs,s:domain(restrictTo(s,xs)) ⊆ xs
prop_restrictTo_domain_leaves_subset :: [VarName] -> Subst -> Bool
prop_restrictTo_domain_leaves_subset xs s = 
  (domain (restrictTo s xs)) `is_subs` xs

-- ∀xs:allVars(restrictTo(empty,xs)) = {}
prop_restrictTo_empty_leaves_empty_vars :: [VarName] -> Bool
prop_restrictTo_empty_leaves_empty_vars xs = 
  allVars (restrictTo empty xs) == []

-- Check all properties in this module:
return []
testAll = $quickCheckAll
