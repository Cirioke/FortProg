module PrettyPrint where

import Type

-- Type class to define the function "pretty".
-- The function "pretty" gives the Prolog representation of an object in Haskell as a string.  
class Pretty a where
  pretty :: a -> String

-- Define "pretty" for "Term".
instance Pretty Term where
  pretty (Var  (VarName a)   ) = a                  
  pretty (Comb "." (a:(b:[]))) = "[" ++ pretty a ++ nexts b ++ "]"
    where
      nexts :: Term -> String
      nexts (Comb "."  (a:(b:[]))) = ", " ++ pretty a ++ nexts b
      nexts (Comb "[]" _         ) = ""
      nexts a                      = "|" ++ pretty a
  pretty (Comb a []) = a  
  pretty (Comb a (b:bs)) = a ++ "(" ++ (foldl (\x b -> x ++ ", " ++ pretty b) (pretty b) bs) ++ ")"

