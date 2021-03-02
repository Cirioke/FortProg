module Variables (Vars, allVars) where

import Type
import SetsAsOrderedList

-- / Vars is a class for datatypes containing variables.
class Vars a where
  -- / Get a list of all variables contained (without dublicates).
  allVars :: a -> [VarName]
  allVars = toList.allVarsSet

  -- / Get a Set of all variables contained.
  allVarsSet :: a -> Set VarName

instance Vars Term where
  allVarsSet (Var   vName         ) = insert vName empty
  allVarsSet (Comb  cName   terms ) = mapUnion allVarsSet terms

instance Vars Rule where
  allVarsSet (Rule conc pres) = mapUnion allVarsSet (conc:pres)

instance Vars Prog where
  allVarsSet (Prog rules) = mapUnion allVarsSet rules

instance Vars Goal where
  allVarsSet (Goal terms) = mapUnion allVarsSet terms



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
    digitCombs = allCombs ['0'..'9']

    -- \ Takes in a string and returns a list of VarNames 
    -- with the given String as Value each extended with a alphabet letter.
    appendLetters :: String -> [VarName]
    appendLetters cs = [VarName (l:cs)| l<-['A'..'Z']]
     
