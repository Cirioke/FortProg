module SLDResolution where

import Data.Maybe

import Type

import Variables
import Substitutions
import Unification
import Renaming


-- 1.
data SLDTree = SLDTree Goal [(Subst,SLDTree)]
 deriving Show

-- 2.
-- / Constructs a sld tree for a given goal and programm.
-- Works only proper for goal and programm in named mode,
-- so all variables in the goal and programm should be named 
-- (no underscore Variables).
sld :: Prog -> Goal -> SLDTree
sld _    goal@(Goal []              ) = SLDTree goal []
sld prog goal@(Goal (literal:terms) ) = SLDTree goal (childs progRenamed)
 where 
  progRenamed = renameNamed (allVars goal) prog

  childs :: Prog -> [(Subst,SLDTree)]
  childs (Prog rules) = 
    do Rule c as <- rules
       mcu       <- maybeToList(unify c literal)
       newGoal   <- [Goal (apply mcu (as ++ terms))]
       return (mcu, sld prog newGoal)

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
-- / Solves for the goal with the given programm,
-- and returs the list of solutions as substitutions in the order
-- the strategy traverses the sld tree.
-- Goal and Programm should be in unnamed mode.
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith prog goal trav = map restrict (trav sldTree)
  where 
    restrict :: Subst -> Subst
    restrict = (flip restrictTo) (allVars goal)
    sldTree :: SLDTree
    sldTree = (uncurry sld) (renameAnonymOnly [] (prog, goal))
