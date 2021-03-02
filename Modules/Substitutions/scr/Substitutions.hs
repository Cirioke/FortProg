module Substitutions where

import Type

data Subst = Subst [(VarName, Term)]
  deriving (Show)


domain :: Subst -> [VarName]
domain (Subst a) = fst (unzip (filter noSelfImage a))
 where
  noSelfImage :: (VarName, Term) -> Bool
  noSelfImage (a,Var b) = a /= b
  noSelfImage (_,_) = False


empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single a b = Subst [(a, b)]

apply :: Subst -> Term -> Term
apply (Subst [])         a          = a
apply a                  (Comb b c) = Comb b (map (apply a) c)  
apply (Subst ((a, b):c)) (Var d)    = if a == d
                                        then b
                                        else apply (Subst c) (Var d)

