module Renaming 
  ( rename ) 
 where

import Type

import Variables
import AnonymVars
import Substitutions

-- \ Helper function:
-- Renaiming variables fullfilling given property
-- by some name contained in the given name list.
-- Variable names given in the first parameter won't be used in renaming.
-- Underscore variables will be ignored.
_rename :: (Vars a, Substitutable a) 
        => (VarName -> Bool)
        -> [VarName]
        -> [VarName]
        -> a 
        -> a
_rename prop fresh lst x = apply subst x
  where 
    vars::[VarName]
    vars = allVars x

    dom::[VarName]
    -- Underscores should not be replaced
    dom = filter (\ x -> x /= (VarName "_") && prop x) vars

    img::[Term]
    img = map Var (filter (not.(`elem` (vars ++ lst))) fresh)

    singleSubsts::[Subst]
    singleSubsts = zipWith single dom img

    subst::Subst
    subst = foldr compose empty singleSubsts


-- \ Renaiming all Variables within (second parameter).
-- Variable names given in the first parameter won't be used in renaming.
-- Underscore variables will be ignored.
-- Anonyme varibables will be renamed to nanonyme variables.
rename :: (Vars a, Substitutable a) => [VarName] -> a -> a
rename lst = (_rename isAnonym freshAnonym lst)
             .
             (_rename (not.isAnonym) freshVars lst)
  

