module Variables 
  ( Vars
  , allVars
  , allVarsSet
  , freshVars
  ) where

import Type
import SetsAsOrderedList

-- 1. --------------------------------------------------------------------------
-- / Vars is a class for datatypes containing variables.
class Vars a where
  -- / Get a list of all variables contained (without dublicates).
  allVars :: a -> [VarName]
  allVars = toList.allVarsSet

  -- / Get a Set of all variables contained.
  allVarsSet :: a -> Set VarName

instance Vars a  => Vars [a] where
  allVarsSet = mapUnion allVarsSet

instance (Vars a, Vars b) => Vars (a,b) where
  allVarsSet (a,b) = union (allVarsSet a) (allVarsSet b)

instance Vars VarName where
  allVarsSet vName = insert vName empty

instance Vars Term where
  allVarsSet (Comb _     terms ) = allVarsSet terms
  allVarsSet (Var  vName       ) = allVarsSet vName

instance Vars Rule where
  allVarsSet (Rule conc pres) = allVarsSet (conc:pres)

instance Vars Prog where
  allVarsSet (Prog rules) = allVarsSet rules

instance Vars Goal where
  allVarsSet (Goal terms) = allVarsSet terms


-- 2. --------------------------------------------------------------------------
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
     
