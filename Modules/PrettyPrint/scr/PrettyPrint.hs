module PrettyPrint where

import Type

-- Type class to define the function "pretty".
-- The method "pretty" returns the Prolog representation
-- of an object in Haskell as a string.  
class Pretty a where
  pretty :: a -> String

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
    


