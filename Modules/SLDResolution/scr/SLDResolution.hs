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
 deriving (Show)                                 -- Debug

-- 2.
sld :: Prog -> Goal -> SLDTree
sld _ g@(Goal []             ) = SLDTree g []
sld p g@(Goal _) = SLDTree gNamed (childs pRenamedNamed)
 where 
  --Modular Version:
--   gNamed = rename [] g
--   pRenamedNamed = rename (allVars g) p

  -- Our Specific Version:
  (gNamed@(Goal (namedLiteral:namedTerms) ), pNamed) = nameAnonym (g, p)
  pRenamedNamed = renameNamed (allVars gNamed) pNamed

  childs :: Prog -> [(Subst,SLDTree)]
  childs (Prog rules) = 
    do Rule c as <- rules
       mcu       <- maybeToList(unify c namedLiteral)
       newGoal   <- [Goal (apply mcu (as ++ namedTerms))]
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
solveWith p g s = map ((flip restrictTo) (allVars g)  ) (s (sld p g)) 

-- Debug vvv ------------------------------------------------------

t = Comb "=" [Var (VarName "X"),Var (VarName "X")]  
p = Prog [Rule t []]
g = Goal [Comb "=" [Var (VarName "_"),Var (VarName "5")]]
--SLDTree (Goal [Comb "=" [Var (VarName "A"),Var (VarName "B")]]) 
--[(Subst [(VarName "A",Var (VarName "B")),(VarName "C",Var (VarName "B"))],SLDTree (Goal []) [])]

-- unify (=(X,X),=(A,B))

-- =(X,X)
-- =(_,5)

--  {X->B, A->B}

-- =(B,B)
-- =(B,B)


-- SLDTree ["=(C, D)"] [({C -> A, A -> B},SLDTree [] [])]
--                      ({C -> A, A -> B},SLDTree [] [])




