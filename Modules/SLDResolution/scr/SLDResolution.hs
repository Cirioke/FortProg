module SLDResolution where

import Data.Maybe
import Substitutions
import SetsAsOrderedList
import Type
import Unification
import Renaming
import Variables

-- 1.
data SLDTree = SLDTree Goal [(Subst,SLDTree)]


-- 2.
sld :: Prog -> Goal -> SLDTree
sld p g = SLDTree g (childs (rename (allVars g) p) g)
 where 
  childs :: Prog -> Goal -> [(Subst,SLDTree)]
  childs prog@(Prog rules) (Goal (literal:terms)) = 
    do Rule c as <- rules
       mcu       <- maybeToList(unify c literal)
       return (mcu, sld prog (Goal (apply mcu (as ++ terms))))  
  childs _ (Goal []) = []


-- 3.
type Strategy = SLDTree -> [Subst]

-- dfs :: Strategy
-- dfs tr 

