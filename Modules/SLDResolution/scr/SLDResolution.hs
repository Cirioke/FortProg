module SLDResolution where

import Data.Maybe

import Type

import PrettyPrint
import Variables
import Substitutions
import SetsAsOrderedList hiding ( empty )
import Unification
import Renaming


-- 1.
data SLDTree = SLDTree Goal [(Subst,SLDTree)]

-- 2.
sld :: Prog -> Goal -> SLDTree
sld _ g@(Goal []             ) = SLDTree g []
sld p g@(Goal (literal:terms)) = SLDTree g (childs renamed_p)
 where 
  renamed_p :: Prog
  renamed_p = rename (allVars g) p

  childs :: Prog -> [(Subst,SLDTree)]
  childs (Prog rules) = 
    do Rule c as <- rules
       mcu       <- maybeToList(unify c literal)
       newGoal   <- [Goal (apply mcu (as ++ terms))]
       return (mcu, sld p newGoal)

-- 3.
type Strategy = SLDTree -> [Subst]

dfs :: Strategy
dfs (SLDTree (Goal []) _      ) = [empty]
dfs (SLDTree _         []     ) = [] 
dfs (SLDTree g         childs ) = concatMap combine childs
 where 
  combine :: (Subst, SLDTree) -> [Subst]
  combine (s,tr) = map  ((flip compose) s) (dfs tr)


-- 4.
bfs :: SLDTree -> [Subst]
bfs (SLDTree (Goal []) _      ) = [empty]
bfs (SLDTree (Goal _ ) childs ) = foldr sortAndCompose [] childs
  where 
    sortAndCompose:: (Subst, SLDTree) -> [Subst] -> [Subst]
    sortAndCompose (sub, tree) subs  = case tree of
      (SLDTree (Goal []) _) -> compose empty sub : subs
      _                     -> subs ++ map ((flip compose) sub) (bfs tree)


-- 5.
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g s = s (sld p g)











