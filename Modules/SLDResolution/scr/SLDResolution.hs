module SLDResolution where

import Data.Maybe
import Substitutions
import SetsAsOrderedList hiding ( empty )
import Type
import Unification
import Renaming
import Variables
import PrettyPrint


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
       return ( mcu, sld prog (Goal (apply mcu (as ++ terms))))  
  childs _ (Goal []) = []

-- 3.
type Strategy = SLDTree -> [Subst]

dfs :: Strategy
dfs (SLDTree (Goal []) _      ) = [empty]
dfs (SLDTree _         []     ) = [] 
dfs (SLDTree g         childs ) = concatMap combine childs
 where 
  combine :: (Subst, SLDTree) -> [Subst]
  combine (s,tr) = map  ((flip compose) s) (dfs tr)



-- t1 = Comb "f" [Var (VarName "B"), Var (VarName "_"), Comb "true" []]
t2 = Comb "." [Comb "true" [], Comb "." [Comb "g" [Var (VarName "C")], Comb "[]" []]]
-- t3 = Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]]

-- f(a,b).
t1 = Comb "f" [Comb "a" [], Comb "b" []]
r1 = Rule t1 []

p = Prog [r1]

g = Goal [Comb "f" [Comb "a" [], Var (VarName "X")]]




















