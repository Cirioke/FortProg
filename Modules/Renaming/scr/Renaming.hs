module Rename where

import Type

import Variables
import Substitutions

class Vars a => Rename a where
  rename :: [VarName] -> a -> a

instance Rename Term where
  rename nonames  



