module Substitutions 
    ( Subst
    , domain
    , empty
    , single
    , apply
    , compose
    , restrictTo
    )
  where

import Test.QuickCheck
import Type
-- import SetsAsOrderedLists
import PrettyPrint
import Variables

-- 1.
data Subst = Subst [(VarName, Term)]
  deriving (Show, Vars) -- Teilaufgabe 8.

-- 2.
domain :: Subst -> [VarName]
domain (Subst a) = fst (unzip (filter noSelfImage a))
 where
  noSelfImage :: (VarName, Term) -> Bool
  noSelfImage (a,Var b) = a /= b
  noSelfImage (_,_) = False


-- 3.
empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single a b = Subst [(a, b)]

-- 4.
apply :: Subst -> Term -> Term
apply (Subst [])         a          = a
apply a                  (Comb b c) = Comb b (map (apply a) c)  
apply (Subst ((a, b):c)) (Var d)    = if a == d
                                        then b
                                        else apply (Subst c) (Var d)

-- 5.
-- compose :: Subst -> Subst -> Subst

-- 6.
-- restrictTo :: Subst -> [VarName] -> Subst


-- 7.
-- instance Pretty Subst where
--   pretty s = 


-- 8.
-- instance Vars Subst where
--   allVarsSet (Subst substRules) = allVarsSet substRules


-- 9.
-- instance Arbitrary Subst where
--   arbitrary = 
