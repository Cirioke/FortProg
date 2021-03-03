{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Type

import Substitutions



-- ∀t:apply(empty,t) = t
prop_empty_neutral :: 
-- ∀x,t:apply(single(x,t),Var x) = t
-- ∀t,s1,s2:apply(compose(s1,s2),t) = apply(s1,apply(s2,t)) 
-- domain(empty) = {}
-- ∀x:domain(single(x,Var x)) = {}
-- ∀x,t:t ≠ Var x ⇒ domain(single(x,t)) = {x}
-- ∀s1,s2:domain(compose(s1,s2)) ⊆ domain(s1) ∪ domain(s2)
-- ∀x1,x2:x1 ≠ x2 ⇒ domain(compose(single(x2,Var x1),
--                   single(x1,Var x2))) = {x2}
-- allVars(empty) = {}
-- ∀x:allVars(single(x,Var x)) = {}
-- ∀x,t:t ≠ Var x ⇒ allVars(single(x,t)) = allVars(t) ∪ {x}
-- ∀s1,s2:allVars(compose(s1,s2)) ⊆ allVars(s1) ∪ allVars(s2)
-- ∀x1,x2:x1 ≠ x2 ⇒ allVars(compose(single(x2,Var x1),single(x1,Var x2)))
--                   = {x1,x2}
-- ∀s:domain(s) ⊆ allVars(s)
-- ∀xs:domain(restrictTo(empty,xs)) = {}
-- ∀xs,s:domain(restrictTo(s,xs)) ⊆ xs

-- Check all properties in this module:
return []
testAll = $quickCheckAll