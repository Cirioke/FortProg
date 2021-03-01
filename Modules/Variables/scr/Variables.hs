module Variables (Vars, allVars) where

import Type

-- / Vars is a class for datatypes containing variables.
class Vars a where
    -- / Get all variables contained (without dublicates).
  allVars :: a -> [VarName]

instance Vars Term where
  allVars (Var   (VarName name)               ) = [VarName "TODO"]
  allVars (Comb           name   (term:terms) ) = [VarName "TODO"]

instance Vars Rule where
  allVars (Rule conc (pre:pres)) = [VarName "TODO"]

instance Vars Prog where
  allVars (Prog (rule:rules)) = [VarName "TODO"]

instance Vars Goal where
  allVars (Goal (term: terms)) = [VarName "TODO"]



-- freshVars :: [VarName]
-- freshVars = (['A'..'Z'] : [fresh ++ nxt | fresh <- freshVars,  nxt <- ['A'..'Z']]) 
--   where 
             