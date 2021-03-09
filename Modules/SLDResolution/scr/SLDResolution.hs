module SLDResolution where

import Data.Maybe

import Type

import SetsAsOrderedList

import Substitutions
import Unification
import Renaming


-- 1.
data SLDTree = SLDTree Goal [(Subst,SLDTree)]

-- 2.
sld :: Prog -> Goal -> SLDTree
sld _ g@(Goal []             ) = SLDTree Goal g
sld p g@(Goal (literal:terms)) = SLDTree g (childs renamed_p)
 where 
  renamed_p :: Prog
  renamed_p = rename (allVars g) p

  childs :: Prog -> [(Subst,SLDTree)]
  childs (Prog rules) = 
    do Rule c as <- rules
       mcu       <- maybeToList(unify c literal)
       newGoal   <- [Goal (apply mcu (as ++ terms)]
       return (mcu, sld p newGoal)


-- 3.
type Strategy = SLDTree -> [Subst]

-- dfs :: Strategy
-- dfs tr 

