module Renaming 
  ( rename) 
 where

import Type

import Variables
import Substitutions

-- \ Renaiming all Variables within (second parameter).
-- Variable names given in the first parameter won't be used in renaming.
-- Underscore variables will be ignored.
-- WARNING: Anonyme varibables will be renamed to non-anonyme variables.
rename :: (Vars a, Substitutable a) => [VarName] -> a -> a
rename lst x = apply subst x
  where 
    dom::[VarName]
    -- Underscores should not be replaced
    dom = filter (/= (VarName "_")) (allVars x)

    img::[Term]
    img = map Var (filter (not.(`elem` (dom ++ lst))) freshVars)

    singleSubsts::[Subst]
    singleSubsts = zipWith single dom img

    subst::Subst
    subst = foldr compose empty singleSubsts

