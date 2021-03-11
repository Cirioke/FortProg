module SLDResolution where

import Data.Maybe

import Type

import Variables
import AnonymVars
import Substitutions
import Unification
import Renaming

-- 1. --------------------------------------------------------------------------
data SLDTree = SLDTree Goal [(Subst,SLDTree)]


-- 2. --------------------------------------------------------------------------
-- \ Computes the SLDTree for a given goal and program.
sld :: Prog -> Goal -> SLDTree
sld _ g@(Goal []             ) = SLDTree g []
sld p g@(Goal _) = SLDTree gNamed (childs pRenamedNamed)
 where 
  (gNamed@(Goal (namedLiteral:namedTerms) ), pNamed) = nameAnonym (g, p)
  pRenamedNamed = renameNamed (allVars gNamed) pNamed

  childs :: Prog -> [(Subst,SLDTree)]
  childs (Prog rules) = 
    do Rule c as <- rules
       mcu       <- maybeToList(unify c namedLiteral)
       newGoal   <- [Goal (apply mcu (as ++ namedTerms))]
       return (mcu, sld p newGoal)


-- 3. --------------------------------------------------------------------------
type Strategy = SLDTree -> [Subst]

-- \ Depth First Search to combine the substitutions in an SLDTree.
dfs :: Strategy
dfs (SLDTree (Goal []) _      ) = [empty]
dfs (SLDTree _         childs ) = concatMap combine childs
 where 
  combine :: (Subst, SLDTree) -> [Subst]
  combine (s,tr) = map  ((flip compose) s) (dfs tr)


-- 4. --------------------------------------------------------------------------
-- \ Breath First Search to combine the substitutions in an SLDTree.
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


-- 5. --------------------------------------------------------------------------
-- \ Returns the substitutions for the variables in goal,
--   for wich the goal would be satisfied.
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g s = (map (filt . ((flip restrictTo) (allVars g))) (s (sld p g)))
 where
  -- \ Checks whether a substituted term is no anonym variable.
  noAnonym :: (VarName, Term) -> Bool
  noAnonym (_,Var v) = not (isNamed v)
  noAnonym _ = True
  
  filt :: Subst -> Subst
  filt = (filtSubst noAnonym)


