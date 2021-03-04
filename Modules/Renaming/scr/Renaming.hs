module Rename where

import Type

import Variables
import Substitutions

class Vars a => Rename a where
  rename :: [VarName] -> a -> a

  renameAll :: a -> a
  renameAll x = rename (allVars x) x

instance Rename Term where
  rename  (VarName name) = 
  



