{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Variables
import Type

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


prop_VarName :: VarName -> Bool
prop_VarName v = allVars v == [v]

unitTests_Term :: [(Term, [VarName])]
unitTests_Term =
    -- 0
  [ ( Var (VarName "A")
    , [VarName "A"] ) 
    -- 1
  , ( Comb "true" []
    , [] ) 
    -- 2
  , ( Comb "[]" []
    , [] ) 
    -- 3
  , ( Comb "f" [Var (VarName "B"), Var (VarName "_"), Comb "true" []]
    , [VarName "B", VarName "_"] ) 
    -- 4
  , ( Comb "." [Comb "true" [], Comb "[]" []]
    , [] ) 
    -- 5
  , ( Comb "." [Comb "true" [], Comb "." [Comb "g" [Var (VarName "C")], Comb "[]" []]]
    , [VarName "C"] ) 
    -- 6
  , ( Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]]
    , [] ) 
    -- 7
  , ( Comb "." [Comb "true" [], Var (VarName "D")]
    , [VarName "D"] ) 
    -- 8
  , ( Comb "." [Var (VarName "E"), Comb "h" [Var (VarName "F"), Comb "i" [Var (VarName "G")]]]
    , [VarName "E", VarName "F", VarName "G"] ) 
    -- 9
  , ( Comb "." [Comb "true" [], Comb "." [Comb "true" [], Comb "true" []]]
    , [] ) 
    -- 10
  , ( Comb "." [Comb "[]" [], Comb "[]" []]
    , [] ) 
    -- 11
  , ( Comb "." [Comb "." [Comb "true" [], Comb "[]" []], Comb "[]" []]
    , [] ) 
    -- 12
  , ( Comb "." [Var (VarName "H")]
    , [VarName "H"] ) 
    -- 13
  , ( Comb "." [Var (VarName "I"), Comb "true" [], Comb "j" [Var (VarName "J")]]
    , [VarName "I", VarName "J"] ) 
    -- 14
  , ( Comb "." [Var (VarName "L"), Comb "." [Var (VarName "K"), Var (VarName "M"), Var (VarName "N"), Var (VarName "O")]]
    , [VarName "K", VarName "L", VarName "M", VarName "N", VarName "O"] ) 
  ]

prop_unit_term :: Int -> Property
prop_unit_term i =    i < (length unitTests_Term)
                      && i >= 0
                      ==> actual == expected
  where
    (term, expected) = unitTests_Term !! i
    actual = allVars term
    
prop_List_2_Term :: Term -> Term -> Bool
prop_List_2_Term t0 t1 = (allVars t0 ++ allVars t1) `is_set_eq` (allVars [t0,t1])



-- Check all properties in this module:
return []
testAll:: IO Bool
testAll = $quickCheckAll