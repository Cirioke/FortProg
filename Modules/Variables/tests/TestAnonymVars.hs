{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Type

import AnonymVars

countV :: (VarName -> Bool) -> Term -> Int
countV p (Var  vName) | p vName = 1
                        | otherwise     = 0
countV p (Comb _  terms) = sum (map (countV p) terms)

-- / Helper funktion to detremine number of unnamed anonym variables
countUnnamed :: Term -> Int
countUnnamed = countV (\v -> (isAnonym v) && not (isNamed v))

-- / Helper funktion to detremine number of named anonym variables
countNamed :: Term -> Int
countNamed = countV isNamed

-- / Helper funktion to detremine number of anonym variables (named and unnamed)
countAnonym :: Term -> Int
countAnonym =  countV isAnonym



-- PROPERTY 0
prop_0 :: Term -> Bool
prop_0 x = countUnnamed (nameAnonym x) == 0


-- PROPERTY 1
prop_1 :: Term -> Bool
prop_1 x =    (countNamed (nameAnonym x) == countUnnamed x)
           && (countNamed (nameAnonym x) == countAnonym  x)

-- PROPERTY 2
prop_2 :: Term -> Bool
prop_2 x = unnameAnonym (nameAnonym x) == x


-- PROPERTY 3
prop_3 :: Term -> Bool
prop_3 x = unnameAnonym x == x


-- Check all properties in this module:
return []
testAll:: IO Bool
testAll = $quickCheckAll