{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Data.Maybe

import Type

import Substitutions
import Unification

-- ∀t:ds(t,t) = {}
prop_same_means_no_dif :: Term -> Bool
prop_same_means_no_dif t = isNothing (ds t t)

-- ∀t1,t2:ds(t1,t2) ≠ {} ⇒ t1 ≠ t2
prop_has_dif_means_not_same :: Term -> Term -> Property
prop_has_dif_means_not_same t1 t2 = (not.isNothing) (ds t1 t2)
                                    ==> t1 /= t2

-- ∀t1,t2:ds(t1,t2) = {} ⇒ unify(t1,t2) ≠ fail ∧ domain(unify(t1,t2)) = {}
prop_no_dif_means_empty_unificator :: Term -> Term -> Property
prop_no_dif_means_empty_unificator t1 t2 = 
  isNothing (ds t1 t1)
  ==> ((not.isNothing) mcu)  && ((domain <$> mcu) == pure [])
 where mcu = unify t1 t1 


-- ∀t1,t2:unify(t1,t2) ≠ fail ⇒ ds(apply(unify(t1,t2),t1),apply(unify(t1,t2),t2)) = {}
prop_unuificator_unifyes :: Term -> Term -> Property
prop_unuificator_unifyes t1 t2 = (not.isNothing) (unify t1 t2)
                                 ==> handleMaybe (unify t1 t2) t1 t2
 where
  handleMaybe :: Maybe Subst -> Term -> Term -> Bool
  handleMaybe Nothing _ _ = False
  handleMaybe (Just s) _t1 _t2 = isNothing (ds (apply s _t1) (apply s _t2))


-- Check all properties in this module:
return []
testAll:: IO Bool
testAll = $quickCheckAll
