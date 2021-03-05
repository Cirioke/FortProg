{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import PrettyPrint
import Type

unitTests_pretty_Term :: [(Term, String)]
unitTests_pretty_Term = 
 [ (Var (VarName "A")
   , "A" )
 , (Comb "true" []
   , "true" )
 , (Comb "[]" []
   , "[]" )
 , (Comb "f" [Var (VarName "B"), Var (VarName "_"), Comb "true" []]
   , "f(B, _, true)" )
 , (Comb "." [Comb "true" [], Comb "[]" []]
   , "[true]" )
 , (Comb "." [Comb "true" [], Comb "." [Comb "g" [Var (VarName "C")], Comb "[]" []]]
   , "[true, g(C)]" )
 , (Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]]
   , "[1, 2, 3]" )
 , (Comb "." [Comb "true" [], Var (VarName "D")]
   , "[true|D]" )
 , (Comb "." [Var (VarName "E"), Comb "h" [Var (VarName "F"), Comb "i" [Var (VarName "G")]]]
   , "[E|h(F, i(G))]" )
 , (Comb "." [Comb "true" [], Comb "." [Comb "true" [], Comb "true" []]]
   , "[true, true|true]" )
 , (Comb "." [Comb "[]" [], Comb "[]" []]
   , "[[]]" )
 , (Comb "." [Comb "." [Comb "true" [], Comb "[]" []], Comb "[]" []]
   , "[[true]]" )
 , (Comb "." [Var (VarName "H")]
   , ".(H)" )
 , (Comb "." [Var (VarName "I"), Comb "true" [], Comb "j" [Var (VarName "J")]]
   , ".(I, true, j(J))" )
 , (Comb "." [Var (VarName "K"), Comb "." [Var (VarName "L"), Var (VarName "M"), Var (VarName "N"), Var (VarName "O")]]
   , "[K|.(L, M, N, O)]" )
 ]

prop_unit_prettyPrint :: Int -> Property
prop_unit_prettyPrint i =    i < (length unitTests_pretty_Term)
                      && i >= 0
                      ==> actual == expected
  where
    (term, expected) = unitTests_pretty_Term !! i
    actual = pretty term



-- Check all properties in this module:
return []
testAll:: IO Bool
testAll = $quickCheckAll
