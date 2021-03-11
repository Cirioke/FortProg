module PrettyPrint 
  ( Pretty
  , pretty
  , join
  ) 
  where

import Type


-- 1. --------------------------------------------------------------------------
-- Type class to define the function "pretty".
-- The method "pretty" returns the Prolog representation
-- of an object in Haskell as a string.  
class Pretty a where
  pretty :: a -> String


-- 2. --------------------------------------------------------------------------
-- \ Helper function to join a list of string together.
join :: String -> [String] -> String
join _ [] = ""
join s (h:l) = h ++ foldl (++) "" (map (\x -> s++x) l)

-- Define "pretty" for "Term".
instance Pretty Term where
  pretty (Var  (VarName vName)        ) = vName
  pretty (Comb "."             [t0,t1]) = "[" ++ pretty t0 ++ nexts t1 ++ "]"     
   where
    nexts :: Term -> String
    nexts (Comb "."  [_t0,_t1]) = ", " ++ pretty _t0 ++ nexts _t1
    nexts (Comb "[]" _      ) = ""
    nexts  term               = "|" ++ pretty term
  
  pretty (Comb cName   []    ) = cName
  pretty (Comb cName   terms ) = cName ++ "(" ++ joined terms ++ ")" 
   where
    joined :: [Term] -> String 
    joined (t:ts) = (foldl (\s t0 -> s ++ ", " ++ pretty t0) (pretty t) ts)  
    joined []     = ""


instance Pretty Rule where
  pretty (Rule conc assumps) =    pretty conc 
                               ++ " :- " 
                               ++ join ", " (map pretty assumps)


instance Pretty Prog where
  pretty (Prog rules) = unlines (map pretty rules)


instance Pretty Goal where
  pretty (Goal terms) = join ", " (map pretty terms)

instance Show a => Pretty (Maybe a) where
  pretty Nothing  = "-"
  pretty (Just s) = show s
