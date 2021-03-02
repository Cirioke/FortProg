{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Variables
import Type

unitTests_allVars_Term :: [(Term, [VarName])]
unitTests_allVars_Term =
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

prop_unit_allVars :: Int -> Property
prop_unit_allVars i =    i < (length unitTests_allVars_Term)
                      && i >= 0
                      ==> actual == expected
  where
    (term, expected) = unitTests_allVars_Term !! i
    actual = allVars term
    



-- Check all properties in this module:
return []
testAll = $quickCheckAll