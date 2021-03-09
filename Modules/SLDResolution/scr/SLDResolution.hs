module SLDResolution where

import Substitutions
import SetsAsOrderedList
import Type

data SLDTree = SLDTree Goal [(Subst,SLDTree)]

genSubs :: [Rule] -> [Term] -> [(Subst, SLDTree)]

genSubs ((Rule rt rts):rs) (Goal (t:ts)) = if isNothing (unify (rename rt) t) 
                                              then 
                                              else 
    
    us = map (\(Rule rt rts) -> unify t rt) (map rename rs)
    
    map (\(s, r) -> app s r ) (zip us (map rename rs))

sld :: Prog -> Goal -> SLDTree
sld (Prog rs) g = SLDTree g (genSubs rs g) -- [(Subst,SLDTree)]
    

    
    
    r = rename(rule)
    u = unify (r.left) g[0]
    
    apply u [r.right] ++ g[1:]

    
    
    








