module Renaming 
  ( Rename
  , rename
  , renameAll
  ) 
 where

import Type

import Variables
import Substitutions

class Vars a => Rename a where
  rename :: [VarName] -> a -> a

  renameAll :: a -> a
  renameAll x = rename (allVars x) x

getNewName :: [VarName] -> VarName
getNewName lst = (fliter (`elem` lst) freshNames) !! 0

-- instance Rename Term where
--   rename lst (VarName name) = VarName (filter (`elem` lst) !! 0)
  



