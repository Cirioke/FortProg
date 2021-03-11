module SLDResolution where

import Data.Maybe

import Type

import Variables
import AnonymVars
import Substitutions
import Unification
import Renaming


-- 1.
data SLDTree = SLDTree Goal [(Subst,SLDTree)]

-- 2.
sld :: Prog -> Goal -> SLDTree
sld _ g@(Goal []             ) = SLDTree g []
sld p g@(Goal (literal:terms)) = SLDTree gNamed (childs pRenamedNamed)
 where 
  --Modular Version:
  gNamed = rename [] g
  pRenamedNamed = rename (allVars g) p

  -- -- Our Specific Version:
  -- (gNamed, pNamed) = nameAnonym (g, p)
  -- pRenamedNamed = renameNamed (allVars gNamed) pNamed

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
dfs (SLDTree _         childs ) = concatMap combine childs
 where 
  combine :: (Subst, SLDTree) -> [Subst]
  combine (s,tr) = map  ((flip compose) s) (dfs tr)


-- 4.
bfs :: Strategy
bfs (SLDTree (Goal []) _      ) = [empty]
bfs (SLDTree _         edges ) = concatMap combine sortedEdges
  where 
    combine :: (Subst, SLDTree) -> [Subst]
    combine (s,tr) = map  ((flip compose) s) (dfs tr)

    sortedEdges :: [(Subst, SLDTree)]
    sortedEdges = (filter toLeaf edges) ++ (filter (not.toLeaf) edges)

    toLeaf :: (Subst, SLDTree) -> Bool
    toLeaf (_, SLDTree _ []) = True
    toLeaf (_, _           ) = False


-- 5.

-- todo: dfs sollte nur listen von singles ausgeben

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g s = filt (s (sld p g))
 where
  filt :: [Subst] -> [Subst]
  filt ss = map f ss
  f :: Subst -> Subst 
  f su =  (restrictTo su (filter (isNamed) (domain su)))















