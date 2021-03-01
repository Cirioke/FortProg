module Variablen (Vars, allVars) where

import Type

-- / Vars is a class for datatypes containing variables.
class Vars a:
    -- / Get all variables contained (without dublicates).
  allVars :: a 
          -> [VarName]

instance Vars Term where
  allVars (Term (Var name)               ) = 
  allVars (Comb  name      (term:terms)  ) = 

instance Vars Rule where
  allVars (Rule conc (pre:pres)) = 

instance Vars Prog where
  allVars (Prog (rule:rules)) = 

instance Vars Goal where
  allVars (Goal (term: terms)) = 



freshVars :: [VarName]
freshVars = (['A'..'Z'] : [fresh ++ nxt | fresh <- freshVars,  nxt <- ['A'..'Z']]) 
  where 
             