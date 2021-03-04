{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Data.Maybe

import Type

import Substitutions
import Unification

-- ∀t:ds(t,t) = {}
prop_same_means_no_dif :: Term -> Bool
prop_same_means_no_dif t = Nothing == ds t t

-- ∀t1,t2:ds(t1,t2) ≠ {} ⇒ t1 ≠ t2
prop_has_dif_means_not_same :: Term -> Term -> Bool
prop_has_dif_means_not_same t1 t2 = if ds t1 t2 /= Nothing
                                     then t1 /= t2
                                     else True

-- ∀t1,t2:ds(t1,t2) = {} ⇒ unify(t1,t2) ≠ fail ∧ domain(unify(t1,t2)) = {}
prop_no_dif_means_empty_unificator :: Term -> Term -> Bool
prop_no_dif_means_empty_unificator t1 t2 = if (ds t1 t2 == Nothing)
                                             then (not (isNothing (unify t1 t2)) ) && handleMaybe2 (unify t1 t2)
                                             else True
 where
  handleMaybe2 :: Maybe Subst -> Bool
  handleMaybe2 Nothing  = False
  handleMaybe2 (Just s) = (domain s) == []


-- ∀t1,t2:unify(t1,t2) ≠ fail ⇒ ds(apply(unify(t1,t2),t1),apply(unify(t1,t2),t2)) = {}
prop_unuificator_unifyes :: Term -> Term -> Bool
prop_unuificator_unifyes t1 t2 = if not (isNothing (unify t1 t2))
                                   then handleMaybe (unify t1 t2) t1 t2
                                   else True
 where
  handleMaybe :: Maybe Subst -> Term -> Term -> Bool
  handleMaybe Nothing _ _ = False
  handleMaybe (Just s) t1 t2 = ds (apply s t1) (apply s t2) == Nothing


-- Check all properties in this module:
return []
testAll = $quickCheckAll
