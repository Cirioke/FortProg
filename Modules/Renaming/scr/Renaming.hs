module Renaming 
  ( Rename
  -- , rename
  , nameAnonym
  , unnameAnonym
  ) 
 where

import Type

import Variables
-- import Substitutions

class Vars a => Rename a where
  -- \ Renaiming all Variables with 
  -- 
  -- rename :: [VarName] -> a -> a
  nameAnonym :: a -> a 
  nameAnonym = _nameAnonym "0"
  _nameAnonym :: String  -- ^ Prefix to use for naming underscores
              -> a       -- ^ Object where to replace underscores
              -> a       -- ^ Object with named anoym variables
  unnameAnonym :: a -> a

 

instance Rename Term where
  _nameAnonym prefix (Var (VarName "_") ) = Var (makeAnonym (VarName prefix))
  _nameAnonym _      (Var  vName        ) = Var vName
  _nameAnonym prefix (Comb cName      ts) = Comb cName (_nameAnonym prefix ts)  
    
  unnameAnonym var@(Var vName) | isAnonym vName = Var (VarName "_")
                               | otherwise      = var
  unnameAnonym (Comb cName terms) = Comb cName (unnameAnonym terms)

instance Rename a => Rename [a] where
  _nameAnonym prefix = zipWith _nameAnonym nodeNames
    where nodeNames = (map ((prefix ++ ).show) [0..])    
    
  unnameAnonym = map unnameAnonym
          


    
  
-- instance Rename Rule where
--   rename lst (Rule c a) = Rule (rename lst c) (map (rename lst) a)

-- instance Rename Prog where
--   rename lst (Prog rs) = Prog (map (rename lst) rs) rs)

-- instance Rename Goal where



