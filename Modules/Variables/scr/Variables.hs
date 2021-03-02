module Variables (Vars, allVars) where

import Type

-- / Vars is a class for datatypes containing variables.
class Vars a where
    -- / Get all variables contained (without dublicates).
  allVars :: a -> [VarName]

insertVarName :: VarName -> [VarName] -> [VarName]
insertVarName vName@(VarName str) lst@( vName0@(VarName str0): vNames) = 
  if str == str0 
    then lst
    else if str < str0
      then vName : lst
      else vName0 : (insertVarName vName vNames)

instance Vars Term where
  allVars (Var   vName                ) = [vName]
  allVars (Comb  cName   (term:terms) ) = [VarName "TODO"]

instance Vars Rule where
  allVars (Rule conc (pre:pres)) = [VarName "TODO"]

instance Vars Prog where
  allVars (Prog (rule:rules)) = [VarName "TODO"]

instance Vars Goal where
  allVars (Goal (term: terms)) = [VarName "TODO"]


-- \ Gives a infinite list of valid Prolog variable names, stored in a VarName.
-- The list has the following format:
-- [ VarName "A"
-- , … 
-- , VarName "Z"
-- , VarName "A0"
-- , …
-- , VarName "Z0"
-- , VarName "A1"
-- , …
-- , VarName "Z1"
-- , …
-- ]
freshVars :: [VarName]
freshVars = concatMap appendLetters digitCombs
  where 
    -- \ Is Returning infinite list of all combinations of elements from
    -- the given list, ascending in the lenght of combinations.
    allCombs :: [a] -> [[a]]
    allCombs []  = [[]]
    allCombs lst = [] : [ x :xs | xs <- (allCombs lst), x <- lst]

    -- \ All possible strings containing only digits.
    digitCombs :: [String]
    digitCombs       = allCombinations     ['0'..'9']

    -- \ Takes in a string and returns a list of VarNames 
    -- with the given String as Value each extended with a alphabet letter.
    appendLetters :: String -> [String]
    appendLetters cs = [VarName (l:cs)| l<-['A'..'Z']]
     
