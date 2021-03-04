{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Type

import Substitutions

-- ∀t:ds(t,t) = {}


-- ∀t1,t2:ds(t1,t2) ≠ {} ⇒ t1 ≠ t


-- 2∀t1,t2:ds(t1,t2) = {} ⇒ unify(t1,t2) ≠ fail ∧ domain(unify(t1,t2)) = {}


-- ∀t1,t2:unify(t1,t2) ≠ fail ⇒ ds(apply(unify(t1,t2),t1),apply(unify(t1,t2),t2)) = {}


-- Check all properties in this module:
return []
testAll = $quickCheckAll