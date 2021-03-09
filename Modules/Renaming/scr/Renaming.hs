module Renaming 
  ( rename
  , renameNamed
  , renameUnnamed
  , renameToNamed
  , renameToUnnamed
  ) 
 where

import Type

import Variables
import AnonymVars
import Substitutions

-- \ Helper function:
-- Renaiming variables -fullfilling given property-
-- by some name contained in the given name list.
-- Variable names given in the first parameter won't be used in renaming.
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
    dom = filter prop vars

    img::[Term]
    img = map Var (filter (not.(`elem` (vars ++ lst))) fresh)

    singleSubsts::[Subst]
    singleSubsts = zipWith single dom img

    subst::Subst
    subst = foldr compose empty singleSubsts


-- \ Renaiming all Variables within (second parameter).
-- Variable names given in the first parameter won't be used in renaming.
-- Underscores will be replaced by some unique anonym variable name.
-- So this fuction should be called only objects in unnamed mode.
rename :: (Vars a, AnonymVars a,  Substitutable a) 
       => [VarName] -> a -> a
rename = renameToNamed
             
-- \ Renaiming all Variables within (second parameter).
-- Variable names given in the first parameter won't be used in renaming.
-- Anonym variables will replaced by fresh anonym variables.
-- So this fuction should be called only objects in named mode.
renameNamed :: (Vars a, AnonymVars a,  Substitutable a) 
            => [VarName] -> a -> a
renameNamed lst =   (_rename (not.isNamed) freshVars   lst) 
                  . (_rename      isNamed  freshAnonym lst)

-- \ Renaiming all Variables within (second parameter).
-- Variable names given in the first parameter won't be used in renaming.
-- Underscore variables stay untouched.
-- So this fuction should be called only objects in unnamed mode.
renameUnnamed :: (Vars a, AnonymVars a,  Substitutable a) 
                => [VarName] -> a -> a
renameUnnamed = _rename (\ v -> v /= VarName "_") freshVars

-- \ Renaiming all Variables within (second parameter).
-- Variable names given in the first parameter won't be used in renaming.
-- Underscores will be replaced by some unique anonym variable name.
-- So this fuction should be called only objects in unnamed mode.
renameToNamed :: (Vars a, AnonymVars a,  Substitutable a) 
                => [VarName] -> a -> a
renameToNamed lst = nameAnonym . (renameUnnamed lst)

-- \ Renaiming all Variables within (second parameter).
-- Variable names given in the first parameter won't be used in renaming.
-- Anonym variables will replaced by underscores.
-- So this fuction should be called only objects in named mode.
renameToUnnamed :: (Vars a, AnonymVars a,  Substitutable a) 
                => [VarName] -> a -> a
renameToUnnamed lst = (renameUnnamed lst) . unnameAnonym

