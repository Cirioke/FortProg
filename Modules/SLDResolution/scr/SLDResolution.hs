module SLDResolution where

import Substitutions
import SetsAsOrderedList
import Type

data SLDTree = SLDTree Goal [(Subst,SLDTree)]


sld :: Prog -> Goal -> SLDTree
sld p g = SLDTree g (sld_childs p g)
 where 
  sld_childs :: Prog -> Goal -> [(Subst,SLDTree)]
  sld_childs prog@(Prog rules) (Goal terms) = 
    do literal   <- terms
       Rule c as <- rules
       mcu       <- unify c literal
       new_goal  <- replace terms literal (apply mcu as)
       return (mcu, sld prog new_goal)


