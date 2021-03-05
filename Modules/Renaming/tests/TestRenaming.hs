{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Type

import Variables
import Renaming

-- \ Some helper function to treat lists like sets,
-- intersection lists all common elements of tow lists
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection as bs = [a | a <-as, a `elem` bs]


-- ∀xs,r:allVars(rename(xs,r)) ∩ allVars(r) = {}
-- Test for Terms
prop_no_common_variables_after_renameing_Term :: [VarName] -> Term -> Bool
prop_no_common_variables_after_renameing_Term xs r = 
  allVars (rename xs r)
  `intersection`
  allVars r
  == []


-- Test for Rules
prop_no_common_variables_after_renameing_Rule :: [VarName] -> Rule -> Bool
prop_no_common_variables_after_renameing_Rule xs r = 
  allVars (rename xs r)
  `intersection`
  allVars r
  == []

-- ∀xs,r:allVars(rename(xs,r)) ∩ xs = {}

-- ∀xs,r:"_" ∉ allVars(rename(xs,r))
-- Test for Terms
prop_underscore_will_be_renamed_Term :: [VarName] -> Term -> Bool
prop_underscore_will_be_renamed_Term xs r = not hasUnderscore
  where
    renamed = rename xs r
    hasUnderscore = (VarName "_") `elem` (allVars renamed)

-- Test for Rules
prop_underscore_will_be_renamed_Rule :: [VarName] -> Rule -> Bool
prop_underscore_will_be_renamed_Rule xs r = not hasUnderscore
  where
    renamed = rename xs r
    hasUnderscore = (VarName "_") `elem` (allVars renamed)

-- ∀xs,r:|allVars(rename(xs,r))| = |allVars(r)|  falls  "_" ∉ allVars(r)
-- Test for Terms
prop_var_num_stays_when_no_anonym_Term :: [VarName] -> Term -> Property
prop_var_num_stays_when_no_anonym_Term xs r = 
  not ((VarName "_") `elem` allVars r)
  ==>
  length (allVars (rename xs r)) == length (allVars r)

-- Test for Rules
prop_var_num_stays_when_no_anonym_Rule :: [VarName] -> Rule -> Property
prop_var_num_stays_when_no_anonym_Rule xs r = 
  not ((VarName "_") `elem` allVars r)
  ==>
  length (allVars (rename xs r)) == length (allVars r)


-- ∀xs,r:|allVars(rename(xs,r))| ≥ |allVars(r)|
-- Test for Terms
prop_var_num_at_most_grows_Term :: [VarName] -> Term -> Bool
prop_var_num_at_most_grows_Term xs r = 
  length (allVars (rename xs r)) >= length (allVars r)

-- Test for Rules
prop_var_num_at_most_grows_Rule :: [VarName] -> Rule -> Bool
prop_var_num_at_most_grows_Rule xs r = 
  length (allVars (rename xs r)) >= length (allVars r)



-- Check all properties in this module:
return []
testAll:: IO Bool
testAll = $quickCheckAll